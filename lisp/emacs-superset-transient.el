;;; emacs-superset-transient.el --- Transient menus for emacs-superset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Transient-based command menus for the emacs-superset orchestrator.

;;; Code:

(require 'transient)
(require 'emacs-superset-core)

;;; Status line for transient header

(defun emacs-superset-transient--status-line ()
  "Return a formatted status line for the transient header."
  (let* ((workspaces (emacs-superset--all-workspaces))
         (total (length workspaces))
         (running (length (seq-filter
                           (lambda (ws)
                             (eq (emacs-superset-workspace-agent-status ws) 'running))
                           workspaces))))
    (format "emacs-superset v%s  |  %d workspace%s  |  %d agent%s running"
            (if (boundp 'emacs-superset-version) emacs-superset-version "?")
            total (if (= total 1) "" "s")
            running (if (= running 1) "" "s"))))

;;; Main transient prefix

;;;###autoload (autoload 'emacs-superset-dispatch "emacs-superset-transient" nil t)
(transient-define-prefix emacs-superset-dispatch ()
  "Emacs Superset - Agent Orchestrator."
  [:description emacs-superset-transient--status-line]
  ["Workspaces"
   ("c" "Create workspace"    emacs-superset-worktree-create)
   ("s" "Switch to workspace" emacs-superset-transient-switch)
   ("d" "Delete workspace"    emacs-superset-transient-delete)
   ("l" "Dashboard"           emacs-superset-dashboard)]
  ["Agent"
   ("a" "Launch agent"          emacs-superset-transient-launch)
   ("k" "Stop agent"            emacs-superset-transient-stop)
   ("t" "Switch to terminal"    emacs-superset-transient-terminal)
   ("x" "Run commands"          emacs-superset-transient-run)]
  ["Review"
   ("r" "Diff vs base branch"     emacs-superset-transient-diff)
   ("u" "Uncommitted changes"     emacs-superset-transient-diff-uncommitted)
   ("m" "Magit status in worktree" emacs-superset-transient-magit)])

;;; Transient wrapper commands
;; These wrap the lower-level functions to provide consistent interactive prompting.

(defun emacs-superset-transient-switch ()
  "Switch to a workspace tab."
  (interactive)
  (emacs-superset-tab-switch
   (emacs-superset--read-workspace "Switch to workspace: ")))

(defun emacs-superset-transient-delete ()
  "Delete a workspace."
  (interactive)
  (emacs-superset-worktree-delete
   (emacs-superset--read-workspace "Delete workspace: ")))

(defun emacs-superset-transient-launch ()
  "Launch an agent in a workspace."
  (interactive)
  (call-interactively #'emacs-superset-agent-launch))

(defun emacs-superset-transient-stop ()
  "Stop an agent."
  (interactive)
  (emacs-superset-agent-stop
   (emacs-superset--read-workspace "Stop agent in: ")))

(defun emacs-superset-transient-terminal ()
  "Switch to an agent terminal."
  (interactive)
  (emacs-superset-agent-switch-to-terminal
   (emacs-superset--read-workspace "Terminal for workspace: ")))

(defun emacs-superset-transient-run ()
  "Run on-demand commands for a workspace."
  (interactive)
  (emacs-superset-config-run-run
   (emacs-superset--read-workspace "Run commands for: ")))

(defun emacs-superset-transient-diff ()
  "Review diff for a workspace."
  (interactive)
  (if (fboundp 'emacs-superset-diff-review)
      (emacs-superset-diff-review
       (emacs-superset--read-workspace "Diff for workspace: "))
    (user-error "emacs-superset-diff not loaded")))

(defun emacs-superset-transient-diff-uncommitted ()
  "Show uncommitted changes for a workspace."
  (interactive)
  (if (fboundp 'emacs-superset-diff-uncommitted)
      (emacs-superset-diff-uncommitted
       (emacs-superset--read-workspace "Uncommitted changes for: "))
    (user-error "emacs-superset-diff not loaded")))

(defun emacs-superset-transient-magit ()
  "Open magit-status for a workspace."
  (interactive)
  (let* ((ws (emacs-superset--read-workspace "Magit status for: "))
         (default-directory (emacs-superset-workspace-path ws)))
    (magit-status-setup-buffer)))

(provide 'emacs-superset-transient)
;;; emacs-superset-transient.el ends here
