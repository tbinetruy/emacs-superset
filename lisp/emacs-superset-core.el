;;; emacs-superset-core.el --- Core data model for emacs-superset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;; emacs-superset is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Data model, customization group, and shared utilities for emacs-superset.

;;; Code:

(require 'cl-lib)
(require 'project)

;;; Customization group

(defgroup emacs-superset nil
  "Agent orchestrator for parallel AI coding workflows."
  :group 'tools
  :prefix "emacs-superset-")

(defcustom emacs-superset-worktree-base-dir "worktrees"
  "Directory name for worktrees, relative to the parent of the repo root.
For example, if the repo is at /code/myproject, worktrees go to
/code/worktrees/myproject/<name>/."
  :type 'string
  :group 'emacs-superset)

(defcustom emacs-superset-default-agent 'claude-code
  "Default agent type to launch in new workspaces."
  :type '(symbol)
  :group 'emacs-superset)

(defcustom emacs-superset-branch-prefix "feat/"
  "Prefix for branches created by emacs-superset."
  :type 'string
  :group 'emacs-superset)

(defcustom emacs-superset-tab-layout 'below
  "Window layout for workspace tabs.
`below' splits with the terminal below the editor.
`right' splits with the terminal to the right."
  :type '(choice (const :tag "Terminal below" below)
                 (const :tag "Terminal right" right))
  :group 'emacs-superset)

(defcustom emacs-superset-terminal-backend 'eat
  "Terminal emulator backend to use.
`eat' uses the eat package (pure Elisp, no C deps).
`vterm' uses vterm (faster, requires libvterm C library)."
  :type '(choice (const :tag "eat" eat)
                 (const :tag "vterm" vterm))
  :group 'emacs-superset)

(defcustom emacs-superset-agent-types
  '((claude-code . "claude")
    (gemini-cli  . "gemini")
    (opencode    . "opencode")
    (aider       . "aider"))
  "Alist mapping agent type symbols to CLI command strings.
Each entry is (SYMBOL . COMMAND) where COMMAND is the executable
name or path to run in the terminal."
  :type '(alist :key-type symbol :value-type string)
  :group 'emacs-superset)

;;; Workspace struct

(cl-defstruct (emacs-superset-workspace (:constructor emacs-superset-workspace-create))
  "Represents one agent workspace backed by a git worktree."
  (path nil :documentation "Absolute path to the worktree directory.")
  (branch nil :documentation "Branch name checked out in this worktree.")
  (name nil :documentation "Human-readable name (defaults to branch sans prefix).")
  (base-branch nil :documentation "Branch this worktree was created from.")
  (tab-name nil :documentation "Name of the tab-bar tab for this workspace.")
  (agent-type nil :documentation "Symbol from `emacs-superset-agent-types'.")
  (agent-status 'idle :documentation "One of: idle, running, waiting, done, error.")
  (agent-buffer nil :documentation "Buffer running the agent terminal.")
  (agent-process nil :documentation "Process object for the agent.")
  (uncommitted 0 :documentation "Number of uncommitted changes.")
  (ahead 0 :documentation "Commits ahead of upstream.")
  (behind 0 :documentation "Commits behind upstream.")
  (created-at nil :documentation "Creation time as float-time.")
  (setup-ran-p nil :documentation "Whether setup hooks have completed.")
  (status-changed-at 0.0 :documentation "Float-time when agent-status last changed."))

;;; Central state

(defvar emacs-superset--workspaces (make-hash-table :test 'equal)
  "Hash table mapping worktree path (string) to `emacs-superset-workspace' struct.")

;;; Helper functions

(defun emacs-superset--repo-root (&optional directory)
  "Return the root of the main git repository.
When in a worktree, this returns the main repo root, not the worktree root.
DIRECTORY defaults to `default-directory'."
  (let* ((dir (or directory default-directory))
         (toplevel (string-trim
                    (with-temp-buffer
                      (let ((default-directory dir))
                        (process-file "git" nil t nil
                                      "rev-parse" "--path-format=absolute"
                                      "--git-common-dir"))
                      (buffer-string)))))
    ;; --git-common-dir returns the .git dir; we want its parent
    (file-name-directory (directory-file-name toplevel))))

(defun emacs-superset--worktree-root (&optional directory)
  "Return the worktree root for DIRECTORY (or `default-directory').
Unlike `emacs-superset--repo-root', this returns the worktree's own root."
  (let ((dir (or directory default-directory)))
    (string-trim
     (with-temp-buffer
       (let ((default-directory dir))
         (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
       (buffer-string)))))

(defun emacs-superset--worktree-base-path (&optional repo-root)
  "Compute the base directory for worktrees.
Given REPO-ROOT (or auto-detected), returns the path where worktrees
are created, based on `emacs-superset-worktree-base-dir'."
  (let* ((root (or repo-root (emacs-superset--repo-root)))
         (repo-name (file-name-nondirectory (directory-file-name root)))
         (parent (file-name-directory (directory-file-name root))))
    (expand-file-name repo-name
                      (expand-file-name emacs-superset-worktree-base-dir parent))))

(defun emacs-superset--all-workspaces ()
  "Return a list of all tracked workspace structs."
  (let (result)
    (maphash (lambda (_path ws) (push ws result))
             emacs-superset--workspaces)
    (sort result (lambda (a b)
                   (string< (emacs-superset-workspace-name a)
                            (emacs-superset-workspace-name b))))))

(defun emacs-superset--normalize-path (path)
  "Normalize PATH by expanding and removing trailing slashes."
  (directory-file-name (expand-file-name path)))

(defun emacs-superset--get-workspace (path)
  "Get workspace struct for PATH, or nil."
  (gethash (emacs-superset--normalize-path path) emacs-superset--workspaces))

(defun emacs-superset--register-workspace (workspace)
  "Register WORKSPACE in the central hash table."
  (puthash (emacs-superset--normalize-path (emacs-superset-workspace-path workspace))
           workspace
           emacs-superset--workspaces))

(defun emacs-superset--unregister-workspace (workspace)
  "Remove WORKSPACE from the central hash table."
  (remhash (emacs-superset--normalize-path (emacs-superset-workspace-path workspace))
           emacs-superset--workspaces))

(defun emacs-superset--read-workspace (prompt)
  "Read a workspace name with PROMPT using `completing-read'.
Returns the workspace struct."
  (let* ((workspaces (emacs-superset--all-workspaces))
         (names (mapcar #'emacs-superset-workspace-name workspaces))
         (name (completing-read prompt names nil t)))
    (cl-find name workspaces
             :key #'emacs-superset-workspace-name
             :test #'equal)))

(defun emacs-superset--current-workspace ()
  "Return the workspace for the current tab, or nil.
Detects by matching the tab name against \"superset:<name>\" or by
matching `default-directory' against a tracked workspace path."
  (or
   ;; Match by tab name
   (when-let* ((tab (tab-bar--current-tab))
               (tab-name (alist-get 'name tab))
               ((string-match "\\`superset:\\(.+\\)\\'" tab-name))
               (ws-name (match-string 1 tab-name)))
     (cl-find ws-name (emacs-superset--all-workspaces)
              :key #'emacs-superset-workspace-name
              :test #'equal))
   ;; Match by default-directory
   (emacs-superset--get-workspace default-directory)))

(defun emacs-superset--read-workspace-or-current (prompt)
  "Return the current workspace if in one, otherwise prompt with PROMPT."
  (or (emacs-superset--current-workspace)
      (emacs-superset--read-workspace prompt)))

;;; Terminal backend abstraction

(defun emacs-superset--term-buf-name (workspace-name)
  "Return the terminal buffer name for WORKSPACE-NAME."
  (format "*term:superset:%s*" workspace-name))

(defun emacs-superset--term-create-shell (buf-name directory)
  "Create a shell terminal buffer named BUF-NAME in DIRECTORY.
Returns the buffer."
  (let ((default-directory (file-name-as-directory directory)))
    (pcase emacs-superset-terminal-backend
      ('eat
       (require 'eat)
       (let ((buf (get-buffer-create buf-name)))
         (with-current-buffer buf
           (setq default-directory (file-name-as-directory directory))
           (eat-mode))
         (eat-exec buf buf-name shell-file-name nil nil)
         buf))
      ('vterm
       (require 'vterm)
       (let ((vterm-shell shell-file-name)
             (vterm-buffer-name buf-name))
         (vterm buf-name))))))

(defun emacs-superset--term-exec (buf-name directory command)
  "Create a terminal buffer named BUF-NAME in DIRECTORY running COMMAND.
Returns the buffer."
  (let ((default-directory (file-name-as-directory directory)))
    (pcase emacs-superset-terminal-backend
      ('eat
       (require 'eat)
       (let ((buf (get-buffer-create buf-name)))
         (with-current-buffer buf
           (setq default-directory (file-name-as-directory directory))
           (unless (derived-mode-p 'eat-mode)
             (eat-mode)))
         (eat-exec buf buf-name shell-file-name nil
                   (list shell-command-switch command))
         buf))
      ('vterm
       (require 'vterm)
       (let* ((vterm-shell (format "%s %s %s" shell-file-name
                                   shell-command-switch command))
              (vterm-buffer-name buf-name))
         (vterm buf-name))))))

(provide 'emacs-superset-core)
;;; emacs-superset-core.el ends here
