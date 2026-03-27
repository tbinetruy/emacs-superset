;;; emacs-superset-dashboard.el --- Workspace monitoring dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; A tabulated-list-mode buffer showing all workspaces with live status,
;; git state, and keybindings for common operations.

;;; Code:

(require 'emacs-superset-core)
(require 'emacs-superset-worktree)
(require 'tabulated-list)

;;; Customization

(defcustom emacs-superset-dashboard-auto-refresh-interval 5
  "Seconds between auto-refresh of the dashboard.
Set to nil to disable auto-refresh."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'emacs-superset)

;;; Dashboard state

(defvar emacs-superset-dashboard--timer nil
  "Timer for auto-refreshing the dashboard.")

;;; Dashboard mode

(defvar emacs-superset-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'emacs-superset-worktree-create)
    (define-key map (kbd "d") #'emacs-superset-dashboard-delete-at-point)
    (define-key map (kbd "RET") #'emacs-superset-dashboard-switch-at-point)
    (define-key map (kbd "a") #'emacs-superset-dashboard-launch-at-point)
    (define-key map (kbd "s") #'emacs-superset-dashboard-stop-at-point)
    (define-key map (kbd "r") #'emacs-superset-dashboard-diff-at-point)
    (define-key map (kbd "t") #'emacs-superset-dashboard-terminal-at-point)
    (define-key map (kbd "m") #'emacs-superset-dashboard-magit-at-point)
    (define-key map (kbd "R") #'emacs-superset-dashboard-refresh-all-git)
    (define-key map (kbd "?") #'emacs-superset-dashboard-help)
    map)
  "Keymap for `emacs-superset-dashboard-mode'.")

(define-derived-mode emacs-superset-dashboard-mode tabulated-list-mode
  "Superset"
  "Major mode for the emacs-superset workspace dashboard.

\\{emacs-superset-dashboard-mode-map}"
  (setq tabulated-list-format
        [("Name"     16 t)
         ("Branch"   24 t)
         ("Agent"    12 t)
         ("Status"    8 t)
         ("Changes"   8 t)
         ("Sync"     10 t)
         ("Path"      0 t)])
  (setq tabulated-list-sort-key '("Name"))
  (setq tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'emacs-superset-dashboard--refresh nil t)
  (tabulated-list-init-header)
  ;; Start auto-refresh timer
  (emacs-superset-dashboard--start-timer)
  ;; Clean up timer when buffer is killed
  (add-hook 'kill-buffer-hook #'emacs-superset-dashboard--stop-timer nil t))

;;; Dashboard buffer

;;;###autoload
(defun emacs-superset-dashboard ()
  "Open the emacs-superset workspace dashboard.
Automatically discovers workspaces from the current repo's worktrees."
  (interactive)
  ;; Auto-discover if the hash table is empty
  (when (zerop (hash-table-count emacs-superset--workspaces))
    (condition-case nil
        (emacs-superset-worktree-reconcile)
      (error nil)))
  (let ((buf (get-buffer-create "*emacs-superset*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'emacs-superset-dashboard-mode)
        (emacs-superset-dashboard-mode))
      (emacs-superset-dashboard--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;; Data refresh

(defun emacs-superset-dashboard--refresh ()
  "Refresh the dashboard data."
  (let ((workspaces (emacs-superset-worktree-list)))
    ;; Refresh git state for each workspace
    (dolist (ws workspaces)
      (condition-case nil
          (emacs-superset-worktree-refresh-git-state ws)
        (error nil))
      ;; Also refresh agent status
      (when (fboundp 'emacs-superset-agent-status)
        (emacs-superset-agent-status ws)))
    ;; Build entries
    (setq tabulated-list-entries
          (mapcar #'emacs-superset-dashboard--make-entry workspaces))))

(defun emacs-superset-dashboard--make-entry (workspace)
  "Create a tabulated-list entry from WORKSPACE."
  (let* ((name (emacs-superset-workspace-name workspace))
         (branch (emacs-superset-workspace-branch workspace))
         (agent (symbol-name (or (emacs-superset-workspace-agent-type workspace) '-)))
         (status (emacs-superset-workspace-agent-status workspace))
         (status-str (emacs-superset-dashboard--format-status status))
         (changes (emacs-superset-workspace-uncommitted workspace))
         (ahead (emacs-superset-workspace-ahead workspace))
         (behind (emacs-superset-workspace-behind workspace))
         (sync-str (format "+%d/-%d" ahead behind))
         (path (abbreviate-file-name
                (emacs-superset-workspace-path workspace))))
    (list (emacs-superset-workspace-path workspace)
          (vector name branch agent status-str
                  (if (zerop changes) "" (number-to-string changes))
                  sync-str path))))

(defun emacs-superset-dashboard--format-status (status)
  "Format agent STATUS symbol for display."
  (pcase status
    ('idle    (propertize "idle"    'face 'shadow))
    ('running (propertize "running" 'face 'success))
    ('done    (propertize "done"    'face 'font-lock-string-face))
    ('error   (propertize "error"   'face 'error))
    (_        (propertize "-"       'face 'shadow))))

;;; Auto-refresh

(defun emacs-superset-dashboard--start-timer ()
  "Start the auto-refresh timer."
  (emacs-superset-dashboard--stop-timer)
  (when emacs-superset-dashboard-auto-refresh-interval
    (setq emacs-superset-dashboard--timer
          (run-with-timer
           emacs-superset-dashboard-auto-refresh-interval
           emacs-superset-dashboard-auto-refresh-interval
           #'emacs-superset-dashboard--auto-refresh))))

(defun emacs-superset-dashboard--stop-timer ()
  "Stop the auto-refresh timer."
  (when emacs-superset-dashboard--timer
    (cancel-timer emacs-superset-dashboard--timer)
    (setq emacs-superset-dashboard--timer nil)))

(defun emacs-superset-dashboard--auto-refresh ()
  "Auto-refresh the dashboard if visible."
  (when-let ((buf (get-buffer "*emacs-superset*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (emacs-superset-dashboard--refresh)
        (tabulated-list-print t t)))))

;;; Interactive commands (operate on workspace at point)

(defun emacs-superset-dashboard--workspace-at-point ()
  "Return the workspace struct for the entry at point."
  (when-let ((id (tabulated-list-get-id)))
    (emacs-superset--get-workspace id)))

(defun emacs-superset-dashboard-switch-at-point ()
  "Switch to the workspace tab at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (emacs-superset-tab-switch ws)
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-delete-at-point ()
  "Delete the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (progn
        (emacs-superset-worktree-delete ws)
        (emacs-superset-dashboard--refresh)
        (tabulated-list-print t))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-launch-at-point ()
  "Launch an agent in the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (if (fboundp 'emacs-superset-agent-launch)
          (call-interactively
           (lambda ()
             (interactive)
             (let* ((type (intern
                           (completing-read
                            "Agent type: "
                            (mapcar (lambda (x) (symbol-name (car x)))
                                    emacs-superset-agent-types)
                            nil t nil nil
                            (symbol-name (or (emacs-superset-workspace-agent-type ws)
                                             emacs-superset-default-agent)))))
                    (prompt (read-string "Initial prompt (empty to skip): ")))
               (emacs-superset-agent-launch ws type
                                            (if (string-empty-p prompt) nil prompt)))))
        (user-error "emacs-superset-agent not loaded"))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-stop-at-point ()
  "Stop the agent in the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (if (fboundp 'emacs-superset-agent-stop)
          (emacs-superset-agent-stop ws)
        (user-error "emacs-superset-agent not loaded"))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-diff-at-point ()
  "Show diff for the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (if (fboundp 'emacs-superset-diff-review)
          (emacs-superset-diff-review ws)
        (user-error "emacs-superset-diff not loaded"))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-terminal-at-point ()
  "Switch to the terminal for the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (if (fboundp 'emacs-superset-agent-switch-to-terminal)
          (emacs-superset-agent-switch-to-terminal ws)
        (user-error "emacs-superset-agent not loaded"))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-magit-at-point ()
  "Open magit-status for the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (let ((default-directory (emacs-superset-workspace-path ws)))
        (magit-status-setup-buffer))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-refresh-all-git ()
  "Refresh git state for all workspaces."
  (interactive)
  (dolist (ws (emacs-superset--all-workspaces))
    (condition-case nil
        (emacs-superset-worktree-refresh-git-state ws)
      (error nil)))
  (emacs-superset-dashboard--refresh)
  (tabulated-list-print t t)
  (message "Git state refreshed"))

(defun emacs-superset-dashboard-help ()
  "Show help for dashboard keybindings."
  (interactive)
  (message "c:create  d:delete  RET:switch  a:agent  s:stop  r:diff  t:terminal  m:magit  R:refresh-git  g:refresh"))

(provide 'emacs-superset-dashboard)
;;; emacs-superset-dashboard.el ends here
