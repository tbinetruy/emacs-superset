;;; emacs-superset-agent.el --- Agent session management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Launch and track AI coding agents in eat terminal buffers, one per workspace.

;;; Code:

(require 'emacs-superset-core)
(require 'eat)

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
  ;; Resolve agent type and command
  (let* ((type (or agent-type
                   (emacs-superset-workspace-agent-type workspace)
                   emacs-superset-default-agent))
         (cmd (or (alist-get type emacs-superset-agent-types)
                  (user-error "Unknown agent type: %s" type)))
         (name (emacs-superset-workspace-name workspace))
         (path (emacs-superset-workspace-path workspace))
         (buf-name (format "*superset:%s*" name)))
    ;; Don't launch if already running
    (when (eq (emacs-superset-workspace-agent-status workspace) 'running)
      (user-error "Agent already running in workspace %s" name))
    ;; Switch to workspace tab if it exists
    (when (emacs-superset-workspace-tab-name workspace)
      (emacs-superset-tab-switch workspace))
    ;; Kill existing placeholder/dead buffer
    (when-let ((old-buf (get-buffer buf-name)))
      (unless (get-buffer-process old-buf)
        (kill-buffer old-buf)))
    ;; Build the full command with optional prompt
    (let* ((full-cmd (if prompt
                         (format "%s %s" cmd (shell-quote-argument prompt))
                       cmd))
           (default-directory (file-name-as-directory path))
           (eat-buf (get-buffer-create buf-name)))
      ;; Initialize eat in the buffer and launch the command
      (with-current-buffer eat-buf
        (unless (derived-mode-p 'eat-mode)
          (eat-mode))
        (setq default-directory (file-name-as-directory path)))
      (eat-exec eat-buf buf-name shell-file-name nil
                (list shell-command-switch full-cmd))
      ;; Update workspace struct
      (setf (emacs-superset-workspace-agent-type workspace) type)
      (setf (emacs-superset-workspace-agent-status workspace) 'running)
      (setf (emacs-superset-workspace-agent-buffer workspace) eat-buf)
      (setf (emacs-superset-workspace-agent-process workspace)
            (get-buffer-process eat-buf))
      ;; Chain our sentinel after eat's own sentinel to track agent exit
      (when-let ((proc (get-buffer-process eat-buf)))
        (let ((eat-sentinel (process-sentinel proc)))
          (set-process-sentinel
           proc
           (emacs-superset-agent--make-sentinel workspace eat-sentinel))))
      ;; Display the terminal in the workspace's terminal window
      (emacs-superset-agent--display-terminal workspace)
      (message "Launched %s in workspace %s" type name))))

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
      (setf (emacs-superset-workspace-agent-status workspace) status)
      (setf (emacs-superset-workspace-agent-process workspace) nil)
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
      (setf (emacs-superset-workspace-agent-status workspace) 'idle)
      (message "No running agent in %s"
               (emacs-superset-workspace-name workspace)))))

;;; Agent status

(defun emacs-superset-agent-status (workspace)
  "Return the current agent status symbol for WORKSPACE.
Also updates the workspace struct if the process has died."
  (let ((proc (emacs-superset-workspace-agent-process workspace)))
    (when (and proc (not (process-live-p proc)))
      (setf (emacs-superset-workspace-agent-status workspace)
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
