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
    ;; Set default-directory to the worktree
    (setq default-directory (file-name-as-directory path))
    ;; Open dired in the worktree root
    (dired path)
    ;; Split window for terminal (will be populated by agent-launch)
    (pcase emacs-superset-tab-layout
      ('below
       (split-window-below)
       (other-window 1)
       ;; Create a placeholder buffer for the terminal slot
       (let ((buf (get-buffer-create (format "*superset:%s*" name))))
         (with-current-buffer buf
           (setq default-directory (file-name-as-directory path))
           (unless (derived-mode-p 'emacs-superset-terminal-placeholder-mode)
             (emacs-superset-terminal-placeholder-mode)))
         (switch-to-buffer buf))
       ;; Go back to the editor window
       (other-window 1))
      ('right
       (split-window-right)
       (other-window 1)
       (let ((buf (get-buffer-create (format "*superset:%s*" name))))
         (with-current-buffer buf
           (setq default-directory (file-name-as-directory path))
           (unless (derived-mode-p 'emacs-superset-terminal-placeholder-mode)
             (emacs-superset-terminal-placeholder-mode)))
         (switch-to-buffer buf))
       (other-window 1)))
    ;; Store the tab name
    (setf (emacs-superset-workspace-tab-name workspace)
          (format "superset:%s" name))))

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
  "Close the tab for WORKSPACE."
  (when-let ((tab-name (emacs-superset-workspace-tab-name workspace)))
    (condition-case nil
        (tab-bar-close-tab-by-name tab-name)
      (error nil))
    (setf (emacs-superset-workspace-tab-name workspace) nil)))

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

(provide 'emacs-superset-tab)
;;; emacs-superset-tab.el ends here
