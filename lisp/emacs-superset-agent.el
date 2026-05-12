;;; emacs-superset-agent.el --- Agent session management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Launch and track AI coding agents in eat terminal buffers, one per workspace.

;;; Code:

(require 'emacs-superset-core)
(require 'json)

;;; Codex integration

(defcustom emacs-superset-codex-json-events t
  "Use `codex exec --json' when launching Codex with an initial prompt.
This lets emacs-superset observe Codex event JSONL and update the dashboard
while the Codex instance changes state.  Launches without an initial prompt use
the interactive Codex TUI."
  :type 'boolean
  :group 'emacs-superset)

(defcustom emacs-superset-codex-session-watch nil
  "Fall back to watching Codex session JSONL files for interactive launches.
Codex hooks are the primary state integration.  When hook installation fails
and this is non-nil, emacs-superset follows the matching session file under
CODEX_HOME/sessions and updates the dashboard from those events."
  :type 'boolean
  :group 'emacs-superset)

(defcustom emacs-superset-codex-session-watch-interval 1
  "Seconds between Codex session file checks."
  :type 'number
  :group 'emacs-superset)

;;; Agent launching

(defun emacs-superset-agent-launch (workspace &optional agent-type prompt)
  "Launch an agent in WORKSPACE's terminal.
AGENT-TYPE overrides the workspace's configured type.
PROMPT is an optional initial prompt string to send to the agent."
  (interactive
   (let* ((ws (emacs-superset--read-workspace-or-current "Launch agent in: "))
          (type (intern
                 (completing-read
                  "Agent type: "
                  (mapcar (lambda (x) (symbol-name (car x)))
                          emacs-superset-agent-types)
                  nil t nil nil
                  (symbol-name (or (emacs-superset-workspace-agent-type ws)
                                   emacs-superset-default-agent)))))
          (p (read-string "Initial prompt (empty to skip): ")))
     (list ws type (if (string-empty-p p) nil p))))
  ;; Resolve agent type
  (let ((type (or agent-type
                  (emacs-superset-workspace-agent-type workspace)
                  emacs-superset-default-agent)))
    ;; If already running, just show the terminal
    (if (eq (emacs-superset-workspace-agent-status workspace) 'running)
        (emacs-superset-agent-switch-to-terminal workspace)
      ;; Resolve command and launch
      (let* ((cmd (or (alist-get type emacs-superset-agent-types)
                      (user-error "Unknown agent type: %s" type)))
             (name (emacs-superset-workspace-name workspace))
             (path (emacs-superset-workspace-path workspace))
             (buf-name (format "*superset:%s*" name))
             (launched-at (float-time))
             (codex-hooks-installed-p
              (and (eq type 'codex)
                   (fboundp 'emacs-superset-hooks-install-codex)
                   (emacs-superset-hooks-install-codex))))
        ;; Switch to workspace tab if it exists
        (when (emacs-superset-workspace-tab-name workspace)
          (emacs-superset-tab-switch workspace))
    ;; Kill existing placeholder/dead buffer
    (when-let ((old-buf (get-buffer buf-name)))
      (unless (get-buffer-process old-buf)
        (kill-buffer old-buf)))
    ;; Build the full command with optional prompt
    (let* ((full-cmd (emacs-superset-agent--build-command type cmd path prompt))
           (term-buf (emacs-superset--term-exec buf-name path full-cmd)))
      ;; Update workspace struct
      (setf (emacs-superset-workspace-agent-type workspace) type)
      (emacs-superset--set-agent-status workspace 'running)
      (setf (emacs-superset-workspace-agent-buffer workspace) term-buf)
      (setf (emacs-superset-workspace-agent-process workspace)
            (get-buffer-process term-buf))
      ;; Chain after the terminal's own handlers to preserve terminal behavior.
      (when-let ((proc (get-buffer-process term-buf)))
        (when (and (eq type 'codex)
                   prompt
                   emacs-superset-codex-json-events)
          (let ((orig-filter (process-filter proc)))
            (set-process-filter
             proc
             (emacs-superset-agent--make-codex-filter workspace orig-filter))))
        (let ((orig-sentinel (process-sentinel proc)))
          (set-process-sentinel
           proc
           (emacs-superset-agent--make-sentinel workspace orig-sentinel))))
      (when (and (eq type 'codex)
                 (not prompt)
                 (not codex-hooks-installed-p)
                 emacs-superset-codex-session-watch)
        (emacs-superset-agent--codex-start-session-watch workspace launched-at))
      ;; Display the terminal in the workspace's terminal window
      (emacs-superset-agent--display-terminal workspace)
      (message "Launched %s in workspace %s" type name))))))

(defun emacs-superset-agent--build-command (agent-type command path prompt)
  "Build shell command for AGENT-TYPE using COMMAND in PATH with PROMPT."
  (cond
   ((and (eq agent-type 'codex) prompt emacs-superset-codex-json-events)
    (format "%s exec --json --cd %s %s"
            command
            (shell-quote-argument path)
            (shell-quote-argument prompt)))
   (prompt
    (format "%s %s" command (shell-quote-argument prompt)))
   ((eq agent-type 'codex)
    (format "%s --no-alt-screen" command))
   (t command)))

(defun emacs-superset-agent--make-sentinel (workspace &optional original-sentinel)
  "Return a process sentinel that updates WORKSPACE on agent exit.
If ORIGINAL-SENTINEL is non-nil, call it first (to preserve eat's sentinel)."
  (lambda (process event)
    ;; Call eat's sentinel first so it can clean up properly
    (when original-sentinel
      (funcall original-sentinel process event))
    (let ((status (cond
                   ((string-match-p "finished" event) 'done)
                   ((string-match-p "\\(killed\\|interrupt\\)" event) 'idle)
                   (t 'error))))
      (emacs-superset--set-agent-status workspace status)
      (setf (emacs-superset-workspace-agent-process workspace) nil)
      (emacs-superset-agent--stop-watch workspace)
      (message "Agent in workspace %s: %s"
               (emacs-superset-workspace-name workspace)
               status)
      ;; Send desktop notification if available
      (when (and (eq status 'done)
                 (fboundp 'notifications-notify))
        (notifications-notify
         :title "emacs-superset"
         :body (format "Agent finished in %s"
                       (emacs-superset-workspace-name workspace)))))))

(defun emacs-superset-agent--make-codex-filter (workspace &optional original-filter)
  "Return a process filter that observes Codex JSONL status for WORKSPACE.
ORIGINAL-FILTER is called first so terminal buffers still receive output."
  (let ((pending ""))
    (lambda (process string)
      (if original-filter
          (funcall original-filter process string)
        (internal-default-process-filter process string))
      (setq pending (concat pending string))
      (let ((lines (split-string pending "\n")))
        (setq pending (if (string-suffix-p "\n" pending) "" (car (last lines))))
        (dolist (line (if (string-empty-p pending) lines (butlast lines)))
          (emacs-superset-agent--handle-codex-event workspace line))))))

(defun emacs-superset-agent--handle-codex-event (workspace line)
  "Update WORKSPACE status from one Codex JSONL event LINE."
  (when (string-prefix-p "{" (string-trim-left line))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-key-type 'symbol)
               (event (json-read-from-string line))
               (type (or (alist-get 'type event)
                         (alist-get 'event event)
                         (alist-get 'msg event)
                         (alist-get 'name event))))
          (when (stringp type)
            (pcase type
              ((or "thread.started" "turn.started" "item.started" "exec.started"
                   "agent.started" "task.started")
               (emacs-superset--set-agent-status workspace 'running))
              ((or "turn.waiting" "approval.requested" "permission.requested"
                   "input.required")
               (emacs-superset--set-agent-status workspace 'waiting))
              ((or "turn.completed" "agent.completed" "task.completed")
               (emacs-superset--set-agent-status workspace 'done))
              ((or "turn.failed" "agent.failed" "task.failed" "error")
               (emacs-superset--set-agent-status workspace 'error)))))
      (error nil))))

(defun emacs-superset-agent--stop-watch (workspace)
  "Stop any agent-specific watcher for WORKSPACE."
  (when-let ((timer (emacs-superset-workspace-agent-watch-timer workspace)))
    (cancel-timer timer)
    (setf (emacs-superset-workspace-agent-watch-timer workspace) nil)))

(defun emacs-superset-agent--codex-home ()
  "Return the Codex home directory."
  (expand-file-name (or (getenv "CODEX_HOME") "~/.codex")))

(defun emacs-superset-agent--codex-sessions-dir ()
  "Return the Codex sessions directory."
  (expand-file-name "sessions" (emacs-superset-agent--codex-home)))

(defun emacs-superset-agent--codex-start-session-watch (workspace launched-at)
  "Watch Codex's session JSONL for WORKSPACE launched at LAUNCHED-AT."
  (emacs-superset-agent--stop-watch workspace)
  (setf (emacs-superset-workspace-agent-session-file workspace) nil)
  (setf (emacs-superset-workspace-agent-session-offset workspace) 0)
  (setf (emacs-superset-workspace-agent-watch-timer workspace)
        (run-with-timer
         0 emacs-superset-codex-session-watch-interval
         #'emacs-superset-agent--codex-session-watch-tick
         workspace launched-at)))

(defun emacs-superset-agent--codex-session-watch-tick (workspace launched-at)
  "Update WORKSPACE status from its Codex session file.
LAUNCHED-AT is used to ignore older sessions in the same directory."
  (condition-case nil
      (when (and (eq (emacs-superset-workspace-agent-type workspace) 'codex)
                 (process-live-p (emacs-superset-workspace-agent-process workspace)))
        (unless (emacs-superset-workspace-agent-session-file workspace)
          (when-let ((file (emacs-superset-agent--codex-find-session-file
                            (emacs-superset-workspace-path workspace)
                            launched-at)))
            (setf (emacs-superset-workspace-agent-session-file workspace) file)))
        (when-let ((file (emacs-superset-workspace-agent-session-file workspace)))
          (emacs-superset-agent--codex-read-session-events workspace file)))
    (error nil)))

(defun emacs-superset-agent--codex-find-session-file (workspace-path launched-at)
  "Return newest Codex session file for WORKSPACE-PATH after LAUNCHED-AT."
  (let ((sessions-dir (emacs-superset-agent--codex-sessions-dir))
        (workspace-path (emacs-superset--normalize-path workspace-path))
        candidates)
    (when (file-directory-p sessions-dir)
      (dolist (file (directory-files-recursively sessions-dir "\\.jsonl\\'"))
        (let* ((attrs (file-attributes file))
               (mtime (float-time (file-attribute-modification-time attrs))))
          (when (>= mtime (- launched-at 5))
            (push (cons file mtime) candidates))))
      (catch 'found
        (dolist (entry (sort candidates (lambda (a b) (> (cdr a) (cdr b)))))
          (when (emacs-superset-agent--codex-session-file-matches-p
                 (car entry) workspace-path)
            (throw 'found (car entry))))))))

(defun emacs-superset-agent--codex-session-file-matches-p (file workspace-path)
  "Return non-nil if Codex session FILE belongs to WORKSPACE-PATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 nil)
        (goto-char (point-min))
        (let* ((json-object-type 'alist)
               (json-key-type 'symbol)
               (event (json-read))
               (payload (alist-get 'payload event))
               (cwd (alist-get 'cwd payload)))
          (and (equal (alist-get 'type event) "session_meta")
               (stringp cwd)
               (equal (emacs-superset--normalize-path cwd) workspace-path))))
    (error nil)))

(defun emacs-superset-agent--codex-read-session-events (workspace file)
  "Read new Codex session events from FILE and update WORKSPACE."
  (when (file-readable-p file)
    (let* ((offset (or (emacs-superset-workspace-agent-session-offset workspace) 0))
           (size (file-attribute-size (file-attributes file))))
      (with-temp-buffer
        (insert-file-contents file nil offset size)
        (setf (emacs-superset-workspace-agent-session-offset workspace) size)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (emacs-superset-agent--handle-codex-session-line workspace line))
          (forward-line 1))))))

(defun emacs-superset-agent--handle-codex-session-line (workspace line)
  "Update WORKSPACE status from one Codex session JSONL LINE."
  (when (string-prefix-p "{" (string-trim-left line))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-key-type 'symbol)
               (event (json-read-from-string line))
               (event-type (alist-get 'type event))
               (payload (alist-get 'payload event))
               (payload-type (alist-get 'type payload)))
          (when (equal event-type "event_msg")
            (pcase payload-type
              ("task_started"
               (emacs-superset--set-agent-status workspace 'running))
              ((or "exec_approval_request" "patch_approval_request")
               (emacs-superset--set-agent-status workspace 'waiting))
              ("task_complete"
               (emacs-superset--set-agent-status workspace 'done))
              ((or "error" "turn_aborted")
               (emacs-superset--set-agent-status workspace 'error)))))
      (error nil))))

(defun emacs-superset-agent--display-terminal (workspace)
  "Display WORKSPACE's agent terminal in the appropriate window."
  (when-let ((buf (emacs-superset-workspace-agent-buffer workspace)))
    (when (buffer-live-p buf)
      ;; Try to find the terminal window in the current tab
      (let ((target-window
             (seq-find (lambda (w)
                         (let ((wbuf (window-buffer w)))
                           (or (string-match-p
                                (regexp-quote (format "*superset:%s*"
                                                     (emacs-superset-workspace-name workspace)))
                                (buffer-name wbuf))
                               (with-current-buffer wbuf
                                 (derived-mode-p 'emacs-superset-terminal-placeholder-mode)))))
                       (window-list))))
        (if target-window
            (set-window-buffer target-window buf)
          ;; Fallback: display in other window
          (display-buffer buf '(display-buffer-use-some-window)))))))

;;; Agent stopping

(defun emacs-superset-agent-stop (workspace)
  "Stop the agent running in WORKSPACE."
  (interactive (list (emacs-superset--read-workspace-or-current "Stop agent in: ")))
  (let ((proc (emacs-superset-workspace-agent-process workspace)))
    (if (and proc (process-live-p proc))
        (progn
          (interrupt-process proc)
          ;; Give it a moment, then kill if still alive
          (run-at-time 2 nil
                       (lambda ()
                         (when (and proc (process-live-p proc))
                           (kill-process proc))))
          (message "Stopping agent in %s..."
                   (emacs-superset-workspace-name workspace)))
      (emacs-superset-agent--stop-watch workspace)
      (emacs-superset--set-agent-status workspace 'idle)
      (message "No running agent in %s"
               (emacs-superset-workspace-name workspace)))))

;;; Agent status

(defun emacs-superset-agent-status (workspace)
  "Return the current agent status symbol for WORKSPACE.
Also updates the workspace struct if the process has died."
  (let ((proc (emacs-superset-workspace-agent-process workspace)))
    (when (and proc (not (process-live-p proc)))
      (emacs-superset--set-agent-status
       workspace
       (if (zerop (process-exit-status proc)) 'done 'error))
      (setf (emacs-superset-workspace-agent-process workspace) nil))
    (emacs-superset-workspace-agent-status workspace)))

;;; Switch to terminal

(defun emacs-superset-agent-switch-to-terminal (workspace)
  "Switch to the agent terminal buffer for WORKSPACE."
  (interactive (list (emacs-superset--read-workspace-or-current "Terminal for workspace: ")))
  ;; First switch to the workspace tab
  (emacs-superset-tab-switch workspace)
  ;; Then select the terminal buffer/window
  (when-let ((buf (emacs-superset-workspace-agent-buffer workspace)))
    (when (buffer-live-p buf)
      (let ((win (get-buffer-window buf)))
        (if win
            (select-window win)
          (pop-to-buffer buf))))))

(provide 'emacs-superset-agent)
;;; emacs-superset-agent.el ends here
