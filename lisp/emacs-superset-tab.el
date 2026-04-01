;;; emacs-superset-tab.el --- Tab-bar integration for workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Each workspace gets its own tab-bar tab with a consistent window layout
;; (editor + terminal split).

;;; Code:

(require 'tab-bar)
(require 'emacs-superset-core)

;;; Tab creation

(defun emacs-superset-tab-create (workspace)
  "Create a new tab for WORKSPACE with the configured window layout."
  (let ((name (emacs-superset-workspace-name workspace))
        (path (emacs-superset-workspace-path workspace)))
    ;; Create a new tab
    (tab-bar-new-tab 1)
    (tab-bar-rename-tab (format "superset:%s" name))
    ;; Open a named terminal in the worktree
    (delete-other-windows)
    (let* ((buf-name (emacs-superset--term-buf-name name))
           (buf (get-buffer buf-name)))
      (if (and buf (buffer-live-p buf))
          (switch-to-buffer buf)
        (switch-to-buffer
         (emacs-superset--term-create-shell buf-name path))))
    ;; Store the tab name
    (setf (emacs-superset-workspace-tab-name workspace)
          (format "superset:%s" name))
    ;; Re-show the dashboard side window if it was open
    (when-let ((dash-buf (get-buffer "*emacs-superset*")))
      (unless (get-buffer-window dash-buf)
        (display-buffer-in-side-window
         dash-buf
         `((side . left)
           (window-width . ,(if (boundp 'emacs-superset-dashboard-sidebar-width)
                                emacs-superset-dashboard-sidebar-width
                              40))
           (window-parameters
            (no-delete-other-windows . t)
            (no-other-window . nil))))))))

;;; Tab switching

(defun emacs-superset-tab-switch (workspace)
  "Switch to the tab for WORKSPACE, creating it if needed."
  (interactive (list (emacs-superset--read-workspace "Switch to workspace: ")))
  (let ((tab-name (emacs-superset-workspace-tab-name workspace)))
    (if tab-name
        (tab-bar-switch-to-tab tab-name)
      (emacs-superset-tab-create workspace)
      (message "Created tab for workspace %s"
               (emacs-superset-workspace-name workspace)))))

;;; Tab closing

(defun emacs-superset-tab-close (workspace)
  "Close the tab for WORKSPACE and kill associated buffers.
Emacs will prompt for confirmation if any buffer has a running process."
  (when-let ((tab-name (emacs-superset-workspace-tab-name workspace)))
    (condition-case nil
        (tab-bar-close-tab-by-name tab-name)
      (error nil))
    (setf (emacs-superset-workspace-tab-name workspace) nil))
  ;; Kill workspace buffers (shell terminal, agent terminal)
  (let ((name (emacs-superset-workspace-name workspace)))
    (dolist (buf-name (list (emacs-superset--term-buf-name name)
                            (format "*superset:%s*" name)))
      (when-let ((buf (get-buffer buf-name)))
        (kill-buffer buf)))))

;;; Placeholder mode for the terminal slot

(define-derived-mode emacs-superset-terminal-placeholder-mode special-mode
  "Superset"
  "Placeholder buffer shown before an agent is launched.
Press \\[emacs-superset-agent-launch-here] to start an agent."
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "emacs-superset" 'face 'bold) "\n\n"
            "No agent running in this workspace.\n\n"
            "Press " (propertize "a" 'face 'bold)
            " in the dashboard or use M-x emacs-superset to launch an agent.\n")))

;;; Tab close hook

(defun emacs-superset-tab--on-close (tab _only-tab-p)
  "Clean up workspace buffers when a superset tab is closed.
TAB is the alist of the closed tab."
  (let ((tab-name (alist-get 'name tab)))
    (when (and tab-name (string-prefix-p "superset:" tab-name))
      (let* ((ws-name (substring tab-name (length "superset:")))
             (term-buf (get-buffer (emacs-superset--term-buf-name ws-name)))
             (agent-buf (get-buffer (format "*superset:%s*" ws-name))))
        ;; Kill workspace buffers
        (when term-buf (kill-buffer term-buf))
        (when agent-buf (kill-buffer agent-buf))
        ;; Clear tab-name on the workspace struct
        (dolist (ws (emacs-superset--all-workspaces))
          (when (equal (emacs-superset-workspace-name ws) ws-name)
            (setf (emacs-superset-workspace-tab-name ws) nil)))))))

(add-hook 'tab-bar-tab-pre-close-functions #'emacs-superset-tab--on-close)

(provide 'emacs-superset-tab)
;;; emacs-superset-tab.el ends here
