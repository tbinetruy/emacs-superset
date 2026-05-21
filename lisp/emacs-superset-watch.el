;;; emacs-superset-watch.el --- Filesystem event watchers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Supervised watchexec processes mark workspaces dirty and schedule async Git
;; state refreshes while the dashboard side window is open.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-superset-core)
(require 'emacs-superset-worktree)

(defcustom emacs-superset-watch-command "watchexec"
  "Executable used to watch workspace filesystem changes."
  :type 'string
  :group 'emacs-superset)

(defcustom emacs-superset-watch-enabled t
  "Whether to use filesystem events for dashboard Git state refreshes."
  :type 'boolean
  :group 'emacs-superset)

(defcustom emacs-superset-watch-debounce 0.3
  "Seconds to debounce watcher events per workspace."
  :type 'number
  :group 'emacs-superset)

(defvar emacs-superset-watch--processes (make-hash-table :test 'equal)
  "Watcher processes keyed by normalized workspace path.")

(defvar emacs-superset-watch--dirty-timers (make-hash-table :test 'equal)
  "Dirty debounce timers keyed by normalized workspace path.")

(defvar emacs-superset-watch--session-disabled nil
  "Non-nil means event watchers are disabled until Emacs restarts.")

(defvar emacs-superset-watch--missing-command-warned nil
  "Whether the missing watchexec warning has been shown.")

(defvar emacs-superset-watch--resolved-command nil
  "Absolute watcher command resolved from Emacs or login shell PATH.")

(defun emacs-superset-watch--dashboard-visible-p ()
  "Return non-nil when the dashboard buffer is currently visible."
  (get-buffer-window "*emacs-superset*" t))

(defun emacs-superset-watch--resolve-command ()
  "Return the executable path for `emacs-superset-watch-command', or nil."
  (or (and (file-name-absolute-p emacs-superset-watch-command)
           (file-executable-p emacs-superset-watch-command)
           emacs-superset-watch-command)
      (executable-find emacs-superset-watch-command)
      (cl-some
       (lambda (switch)
         (let ((found
                (with-temp-buffer
                  (when (zerop (process-file
                                shell-file-name nil t nil
                                switch
                                (format "command -v %s 2>/dev/null"
                                        (shell-quote-argument
                                         emacs-superset-watch-command))))
                    (string-trim (buffer-string))))))
           (unless (or (null found) (string-empty-p found))
             found)))
       (delete-dups (list "-lc" shell-command-switch)))))

(defun emacs-superset-watch--available-p ()
  "Return non-nil when watcher support should be used."
  (and emacs-superset-watch-enabled
       (not emacs-superset-watch--session-disabled)
       (or (setq emacs-superset-watch--resolved-command
                 (emacs-superset-watch--resolve-command))
           (progn
             (setq emacs-superset-watch--session-disabled t)
             (unless emacs-superset-watch--missing-command-warned
               (setq emacs-superset-watch--missing-command-warned t)
               (display-warning
                'emacs-superset
                (format
                 (concat "Cannot find %s; Git event watchers disabled for this session. "
                         "Install watchexec or set `emacs-superset-watch-command' "
                         "to its absolute path; in Doom Emacs, also check exec-path/PATH.")
                 emacs-superset-watch-command)
                :warning))
             nil))))

(defun emacs-superset-watch--git-dir (workspace)
  "Return absolute Git directory path for WORKSPACE, or nil on failure."
  (let ((default-directory (file-name-as-directory
                            (emacs-superset-workspace-path workspace))))
    (when (file-directory-p default-directory)
      (let ((output (with-temp-buffer
                      (when (zerop (process-file
                                    "git" nil t nil "rev-parse"
                                    "--path-format=absolute" "--git-dir"))
                        (string-trim (buffer-string))))))
        (unless (string-empty-p output)
          (expand-file-name output))))))

(defun emacs-superset-watch--debounce-argument ()
  "Return watchexec debounce argument for `emacs-superset-watch-debounce'."
  (format "%dms" (round (* 1000 emacs-superset-watch-debounce))))

(defun emacs-superset-watch--command (kind path)
  "Return watchexec command list for watcher KIND over PATH."
  (append
   (list (or emacs-superset-watch--resolved-command
             emacs-superset-watch-command)
         "--only-emit-events"
         "--postpone"
         "--debounce"
         (emacs-superset-watch--debounce-argument))
   (when (eq kind 'gitdir)
     (list "--no-default-ignore" "--no-vcs-ignore"))
   (list "-w" path)))

(defun emacs-superset-watch--process-key (path kind)
  "Return process hash key for workspace PATH and watcher KIND."
  (cons (emacs-superset--normalize-path path) kind))

(defun emacs-superset-watch-start-workspace (workspace)
  "Start watcher processes for WORKSPACE if the dashboard is visible."
  (when (and (emacs-superset-watch--available-p)
             (emacs-superset-watch--dashboard-visible-p))
    (let* ((path (emacs-superset--normalize-path
                  (emacs-superset-workspace-path workspace)))
           (gitdir (emacs-superset-watch--git-dir workspace)))
      (when (file-directory-p path)
        (emacs-superset-watch--start-process workspace 'worktree path))
      (when gitdir
        (emacs-superset-watch--start-process workspace 'gitdir gitdir)))))

(defun emacs-superset-watch--start-process (workspace kind watch-path)
  "Start a watcher process of KIND for WORKSPACE watching WATCH-PATH."
  (let* ((path (emacs-superset--normalize-path
                (emacs-superset-workspace-path workspace)))
         (key (emacs-superset-watch--process-key path kind))
         (existing (gethash key emacs-superset-watch--processes)))
    (unless (and existing (process-live-p existing))
      (let* ((name (format "emacs-superset-watch:%s:%s"
                           (emacs-superset-workspace-name workspace) kind))
             (buffer (generate-new-buffer (format " *%s*" name)))
             (process
              (make-process
               :name name
               :buffer buffer
               :noquery t
               :connection-type 'pipe
               :command (emacs-superset-watch--command kind watch-path)
               :filter #'emacs-superset-watch--process-filter
               :sentinel #'emacs-superset-watch--process-sentinel)))
        (process-put process 'emacs-superset-watch-key key)
        (process-put process 'emacs-superset-workspace-path path)
        (process-put process 'emacs-superset-watch-kind kind)
        (process-put process 'emacs-superset-watch-stopping nil)
        (puthash key process emacs-superset-watch--processes)
        process))))

(defun emacs-superset-watch--process-filter (process output)
  "Handle watcher PROCESS stdout OUTPUT."
  (when (and (process-live-p process)
             (string-match-p "[^[:space:]]" output))
    (when-let ((path (process-get process 'emacs-superset-workspace-path)))
      (emacs-superset-watch--mark-dirty path))))

(defun emacs-superset-watch--process-sentinel (process _event)
  "Clean up watcher PROCESS and restart it if appropriate."
  (unless (process-live-p process)
    (let* ((key (process-get process 'emacs-superset-watch-key))
           (path (process-get process 'emacs-superset-workspace-path))
           (kind (process-get process 'emacs-superset-watch-kind))
           (stopping (process-get process 'emacs-superset-watch-stopping))
           (buffer (process-buffer process)))
      (when key
        (remhash key emacs-superset-watch--processes))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and (not stopping)
                 path
                 kind
                 (emacs-superset-watch--dashboard-visible-p)
                 (emacs-superset-watch--available-p))
        (run-at-time
         1 nil
         (lambda ()
           (when-let ((workspace (emacs-superset--get-workspace path)))
             (emacs-superset-watch--restart-kind workspace kind))))))))

(defun emacs-superset-watch--restart-kind (workspace kind)
  "Restart watcher KIND for WORKSPACE."
  (when (and (emacs-superset-watch--dashboard-visible-p)
             (emacs-superset-watch--available-p))
    (pcase kind
      ('worktree
       (let ((path (emacs-superset--normalize-path
                    (emacs-superset-workspace-path workspace))))
         (when (file-directory-p path)
           (emacs-superset-watch--start-process workspace kind path))))
      ('gitdir
       (when-let ((gitdir (emacs-superset-watch--git-dir workspace)))
         (emacs-superset-watch--start-process workspace kind gitdir))))))

(defun emacs-superset-watch--mark-dirty (workspace-or-path)
  "Mark WORKSPACE-OR-PATH dirty and schedule one async Git refresh."
  (let* ((path (emacs-superset--normalize-path
                (if (emacs-superset-workspace-p workspace-or-path)
                    (emacs-superset-workspace-path workspace-or-path)
                  workspace-or-path)))
         (old (gethash path emacs-superset-watch--dirty-timers)))
    (when old
      (cancel-timer old))
    (puthash
     path
     (run-at-time emacs-superset-watch-debounce nil
                  #'emacs-superset-watch--refresh-dirty path)
     emacs-superset-watch--dirty-timers)))

(defun emacs-superset-watch--refresh-dirty (path)
  "Refresh Git state for dirty workspace PATH."
  (remhash path emacs-superset-watch--dirty-timers)
  (when-let ((workspace (emacs-superset--get-workspace path)))
    (emacs-superset-worktree-refresh-git-state-async
     workspace
     (lambda (_workspace)
       (when (fboundp 'emacs-superset-dashboard-request-redraw)
         (emacs-superset-dashboard-request-redraw))))))

;;;###autoload
(defun emacs-superset-watch-start ()
  "Start filesystem event watchers for all tracked workspaces."
  (interactive)
  (when (emacs-superset-watch--available-p)
    (dolist (workspace (emacs-superset--all-workspaces))
      (emacs-superset-watch-start-workspace workspace))
    (when (called-interactively-p 'interactive)
      (message "emacs-superset watchers started"))))

(defun emacs-superset-watch-stop-workspace (workspace-or-path)
  "Stop watcher processes and dirty timer for WORKSPACE-OR-PATH."
  (let ((path (emacs-superset--normalize-path
               (if (emacs-superset-workspace-p workspace-or-path)
                   (emacs-superset-workspace-path workspace-or-path)
                 workspace-or-path))))
    (dolist (kind '(worktree gitdir))
      (let* ((key (emacs-superset-watch--process-key path kind))
             (process (gethash key emacs-superset-watch--processes)))
        (when process
          (process-put process 'emacs-superset-watch-stopping t)
          (remhash key emacs-superset-watch--processes)
          (when (process-live-p process)
            (delete-process process))
          (when-let ((buffer (process-buffer process)))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))))
    (when-let ((timer (gethash path emacs-superset-watch--dirty-timers)))
      (cancel-timer timer)
      (remhash path emacs-superset-watch--dirty-timers)))))

;;;###autoload
(defun emacs-superset-watch-stop ()
  "Stop all filesystem event watchers."
  (interactive)
  (maphash
   (lambda (_key process)
     (process-put process 'emacs-superset-watch-stopping t)
     (when (process-live-p process)
       (delete-process process))
     (when-let ((buffer (process-buffer process)))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))
   emacs-superset-watch--processes)
  (clrhash emacs-superset-watch--processes)
  (maphash (lambda (_path timer) (cancel-timer timer))
           emacs-superset-watch--dirty-timers)
  (clrhash emacs-superset-watch--dirty-timers)
  (when (called-interactively-p 'interactive)
    (message "emacs-superset watchers stopped")))

;;;###autoload
(defun emacs-superset-watch-restart ()
  "Restart filesystem event watchers for tracked workspaces."
  (interactive)
  (emacs-superset-watch-stop)
  (setq emacs-superset-watch--session-disabled nil
        emacs-superset-watch--missing-command-warned nil
        emacs-superset-watch--resolved-command nil)
  (emacs-superset-watch-start))

;;;###autoload
(defun emacs-superset-watch-status ()
  "Display filesystem watcher status."
  (interactive)
  (let ((count 0))
    (maphash (lambda (_key process)
               (when (process-live-p process)
                 (cl-incf count)))
             emacs-superset-watch--processes)
    (message "emacs-superset watchers: %d process(es)%s"
             count
             (if emacs-superset-watch--session-disabled
                 " (disabled for this session)"
               ""))))

(provide 'emacs-superset-watch)
;;; emacs-superset-watch.el ends here
