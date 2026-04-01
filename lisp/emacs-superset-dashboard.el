;;; emacs-superset-dashboard.el --- Workspace monitoring dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; A magit-section-mode buffer showing all workspaces with collapsible
;; sections, live status indicators, and keybindings for common operations.

;;; Code:

(require 'emacs-superset-core)
(require 'emacs-superset-worktree)
(require 'magit-section)

;;; Customization

(defcustom emacs-superset-dashboard-auto-refresh-interval 5
  "Seconds between auto-refresh of the dashboard.
Set to nil to disable auto-refresh."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'emacs-superset)

(defcustom emacs-superset-dashboard-sidebar-width 40
  "Width of the dashboard side window in columns."
  :type 'integer
  :group 'emacs-superset)

;;; Faces

(defface emacs-superset-workspace-heading
  '((t :inherit magit-section-heading))
  "Face for workspace names in the dashboard."
  :group 'emacs-superset)

(defface emacs-superset-status-running
  '((t :inherit success))
  "Face for running status indicator."
  :group 'emacs-superset)

(defface emacs-superset-status-done
  '((t :inherit font-lock-string-face))
  "Face for done status indicator."
  :group 'emacs-superset)

(defface emacs-superset-status-waiting
  '((t :inherit warning))
  "Face for waiting status indicator."
  :group 'emacs-superset)

(defface emacs-superset-status-error
  '((t :inherit error))
  "Face for error status indicator."
  :group 'emacs-superset)

(defface emacs-superset-status-idle
  '((t :inherit shadow))
  "Face for idle status indicator."
  :group 'emacs-superset)

(defface emacs-superset-detail
  '((t :inherit shadow))
  "Face for detail lines (branch, stats)."
  :group 'emacs-superset)

;;; Status formatting

(defun emacs-superset-dashboard--status-indicator (status)
  "Return a colored status string for STATUS."
  (pcase status
    ('running (propertize "● running" 'face 'emacs-superset-status-running))
    ('done    (propertize "✓ done"    'face 'emacs-superset-status-done))
    ('waiting (propertize "◌ waiting" 'face 'emacs-superset-status-waiting))
    ('error   (propertize "✗ error"   'face 'emacs-superset-status-error))
    ('idle    (propertize "○ idle"    'face 'emacs-superset-status-idle))
    (_        (propertize "○ idle"    'face 'emacs-superset-status-idle))))

;;; Dashboard state

(defvar emacs-superset-dashboard--timer nil
  "Timer for auto-refreshing the dashboard.")

;;; Dashboard mode

(defvar emacs-superset-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "c") #'emacs-superset-worktree-create)
    (define-key map (kbd "b") #'emacs-superset-worktree-create-from-branch)
    (define-key map (kbd "d") #'emacs-superset-dashboard-delete-at-point)
    (define-key map (kbd "RET") #'emacs-superset-dashboard-switch-at-point)
    (define-key map (kbd "a") #'emacs-superset-dashboard-launch-at-point)
    (define-key map (kbd "s") #'emacs-superset-dashboard-stop-at-point)
    (define-key map (kbd "r") #'emacs-superset-dashboard-diff-at-point)
    (define-key map (kbd "t") #'emacs-superset-dashboard-terminal-at-point)
    (define-key map (kbd "m") #'emacs-superset-dashboard-magit-at-point)
    (define-key map (kbd "R") #'emacs-superset-dashboard-refresh-all-git)
    (define-key map (kbd "g") #'emacs-superset-dashboard-refresh)
    (define-key map (kbd "q") #'emacs-superset-dashboard-close)
    (define-key map (kbd "?") #'emacs-superset-dashboard-help)
    map)
  "Keymap for `emacs-superset-dashboard-mode'.")

(define-derived-mode emacs-superset-dashboard-mode magit-section-mode "Superset"
  "Major mode for the emacs-superset workspace dashboard.

\\{emacs-superset-dashboard-mode-map}"
  ;; Evil bindings
  (when (bound-and-true-p evil-mode)
    (evil-define-key* '(normal motion) emacs-superset-dashboard-mode-map
      (kbd "RET") #'emacs-superset-dashboard-switch-at-point
      (kbd "g r") #'emacs-superset-dashboard-refresh
      (kbd "g R") #'emacs-superset-dashboard-refresh-all-git))
  ;; Start auto-refresh timer
  (emacs-superset-dashboard--start-timer)
  ;; Clean up timer when buffer is killed
  (add-hook 'kill-buffer-hook #'emacs-superset-dashboard--stop-timer nil t))

;;; Dashboard buffer

;;;###autoload
(defun emacs-superset-dashboard ()
  "Open the emacs-superset workspace dashboard as a persistent side window.
If already visible, select it. Otherwise, create it on the left side."
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
      (emacs-superset-dashboard--do-refresh t))
    ;; Show as a persistent left side window
    (let ((win (display-buffer-in-side-window
                buf
                `((side . left)
                  (window-width . ,emacs-superset-dashboard-sidebar-width)
                  (window-parameters
                   (no-delete-other-windows . t)
                   (no-other-window . nil))))))
      (select-window win))))

(defun emacs-superset-dashboard-close ()
  "Close the dashboard side window."
  (interactive)
  (when-let ((buf (get-buffer "*emacs-superset*")))
    (when-let ((win (get-buffer-window buf t)))
      (delete-window win))))

(defun emacs-superset-dashboard-toggle ()
  "Toggle the dashboard side window."
  (interactive)
  (if-let ((buf (get-buffer "*emacs-superset*"))
           (win (get-buffer-window buf t)))
      (delete-window win)
    (emacs-superset-dashboard)))

;;; Section insertion

(defun emacs-superset-dashboard--insert-repo ()
  "Insert a section for the main repository."
  (condition-case nil
      (let* ((repo-root (emacs-superset--repo-root))
             (repo-name (file-name-nondirectory (directory-file-name repo-root)))
             (branch (emacs-superset-worktree--branch-at repo-root)))
        (magit-insert-section (superset-repo repo-root)
          (magit-insert-heading
            (propertize "Repository" 'face 'magit-section-heading)
            "\n")
          (insert (propertize (format "  %s" repo-name)
                              'face 'emacs-superset-workspace-heading)
                  (propertize (format "  (%s)\n\n" branch)
                              'face 'emacs-superset-detail))))
    (error nil)))

(defun emacs-superset-dashboard--insert (workspaces)
  "Insert all WORKSPACES as magit sections."
  (let* ((running (length (seq-filter
                           (lambda (ws)
                             (eq (emacs-superset-workspace-agent-status ws) 'running))
                           workspaces)))
         (waiting (length (seq-filter
                           (lambda (ws)
                             (eq (emacs-superset-workspace-agent-status ws) 'waiting))
                           workspaces)))
         (done (length (seq-filter
                        (lambda (ws)
                          (eq (emacs-superset-workspace-agent-status ws) 'done))
                        workspaces)))
         (parts nil))
    (when (> running 0) (push (format "%d running" running) parts))
    (when (> waiting 0) (push (format "%d waiting" waiting) parts))
    (when (> done 0) (push (format "%d done" done) parts))
    (magit-insert-section (superset-workspaces)
      (magit-insert-heading
        (propertize (format "Workspaces (%d)"  (length workspaces))
                    'face 'magit-section-heading)
        (when parts
          (concat "  " (propertize (string-join (nreverse parts) ", ")
                                   'face 'shadow)))
        "\n")
      (if workspaces
          (dolist (ws workspaces)
            (emacs-superset-dashboard--insert-workspace ws))
        (insert (propertize "  No workspaces found. Press c to create one.\n"
                            'face 'shadow))))))

(defun emacs-superset-dashboard--insert-workspace (workspace)
  "Insert a section for WORKSPACE."
  (let* ((name (emacs-superset-workspace-name workspace))
         (status (emacs-superset-workspace-agent-status workspace))
         (status-str (emacs-superset-dashboard--status-indicator status))
         (branch (or (emacs-superset-workspace-branch workspace) ""))
         (uncommitted (emacs-superset-workspace-uncommitted workspace))
         (ahead (emacs-superset-workspace-ahead workspace))
         (behind (emacs-superset-workspace-behind workspace)))
    (magit-insert-section (superset-workspace workspace)
      (magit-insert-heading
        status-str
        "  "
        (propertize name 'face 'emacs-superset-workspace-heading)
        "\n")
      ;; Body: detail lines
      (insert (propertize (format "  branch: %s\n" branch)
                          'face 'emacs-superset-detail))
      (insert (propertize (format "  sync:   +%d/-%d" ahead behind)
                          'face 'emacs-superset-detail))
      (when (> uncommitted 0)
        (insert (propertize (format "  |  %d uncommitted" uncommitted)
                            'face 'emacs-superset-detail)))
      (insert "\n")
      ;; Agent type if set
      (when-let ((agent-type (emacs-superset-workspace-agent-type workspace)))
        (insert (propertize (format "  agent:  %s\n" agent-type)
                            'face 'emacs-superset-detail)))
      (insert "\n"))))

;;; Refresh

(defun emacs-superset-dashboard-refresh ()
  "Full refresh: reconcile worktrees, refresh git state, redraw."
  (interactive)
  (emacs-superset-dashboard--do-refresh t))

(defun emacs-superset-dashboard--do-refresh (full-git-p)
  "Refresh dashboard. If FULL-GIT-P, also refresh git state."
  (let ((workspaces (emacs-superset-worktree-list)))
    (when full-git-p
      (dolist (ws workspaces)
        (condition-case nil
            (emacs-superset-worktree-refresh-git-state ws)
          (error nil))))
    ;; Refresh agent status
    (dolist (ws workspaces)
      (when (fboundp 'emacs-superset-agent-status)
        (emacs-superset-agent-status ws)))
    ;; Redraw buffer
    (when-let ((buf (get-buffer "*emacs-superset*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (magit-insert-section (superset-root)
            (emacs-superset-dashboard--insert-repo)
            (emacs-superset-dashboard--insert workspaces))
          (goto-char (min pos (point-max))))))))

(defun emacs-superset-dashboard-redraw ()
  "Lightweight redraw from current workspace state.
Skips git state refresh — just rebuilds sections from struct data."
  (emacs-superset-dashboard--do-refresh nil))

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
  "Auto-refresh the dashboard if it exists."
  (when (get-buffer "*emacs-superset*")
    (emacs-superset-dashboard--do-refresh t)))

;;; Interactive commands (operate on workspace at point)

(defun emacs-superset-dashboard--workspace-at-point ()
  "Return the workspace struct for the section at point."
  (when-let ((section (magit-current-section)))
    (and (eq (oref section type) 'superset-workspace)
         (oref section value))))

(defun emacs-superset-dashboard-switch-at-point ()
  "Switch to the workspace or repo at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (emacs-superset-tab-switch ws)
    ;; Check if on the repo section
    (when-let ((section (magit-current-section)))
      (if (eq (oref section type) 'superset-repo)
          ;; Switch to the first non-superset tab
          (let* ((tabs (funcall tab-bar-tabs-function))
                 (non-superset (seq-find
                                (lambda (tab)
                                  (not (string-prefix-p "superset:"
                                        (alist-get 'name tab))))
                                tabs)))
            (if non-superset
                (tab-bar-switch-to-tab (alist-get 'name non-superset))
              (tab-bar-new-tab)
              (tab-bar-rename-tab "main")
              (let ((default-directory (oref section value)))
                (dired default-directory))))
        (user-error "No workspace at point")))))

(defun emacs-superset-dashboard-delete-at-point ()
  "Delete the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (progn
        (emacs-superset-worktree-delete ws)
        (emacs-superset-dashboard-refresh))
    (user-error "No workspace at point")))

(defun emacs-superset-dashboard-launch-at-point ()
  "Launch an agent in the workspace at point."
  (interactive)
  (if-let ((ws (emacs-superset-dashboard--workspace-at-point)))
      (if (fboundp 'emacs-superset-agent-launch)
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
                                         (if (string-empty-p prompt) nil prompt)))
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
  (emacs-superset-dashboard--do-refresh t)
  (message "Git state refreshed"))

(defun emacs-superset-dashboard-help ()
  "Show help for dashboard keybindings."
  (interactive)
  (message "RET:switch  c:create  b:from-branch  d:delete  a:agent  s:stop  r:diff  t:terminal  m:magit  g:refresh  TAB:toggle"))

(provide 'emacs-superset-dashboard)
;;; emacs-superset-dashboard.el ends here
