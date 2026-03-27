;;; emacs-superset-diff.el --- Diff review via magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Wrappers around magit-diff for reviewing agent changes per workspace.

;;; Code:

(require 'emacs-superset-core)
(require 'magit)

(defun emacs-superset-diff-review (workspace)
  "Show diff of WORKSPACE branch vs its base branch using magit."
  (interactive (list (emacs-superset--read-workspace-or-current "Review diff for: ")))
  (let* ((default-directory (emacs-superset-workspace-path workspace))
         (base (or (emacs-superset-workspace-base-branch workspace) "main"))
         (branch (emacs-superset-workspace-branch workspace))
         (range (format "%s...%s" base branch)))
    (magit-diff-range range)))

(defun emacs-superset-diff-uncommitted (workspace)
  "Show uncommitted (unstaged) changes in WORKSPACE."
  (interactive (list (emacs-superset--read-workspace-or-current "Uncommitted changes for: ")))
  (let ((default-directory (emacs-superset-workspace-path workspace)))
    (magit-diff-unstaged)))

(defun emacs-superset-diff-staged (workspace)
  "Show staged changes in WORKSPACE."
  (interactive (list (emacs-superset--read-workspace-or-current "Staged changes for: ")))
  (let ((default-directory (emacs-superset-workspace-path workspace)))
    (magit-diff-staged)))

(provide 'emacs-superset-diff)
;;; emacs-superset-diff.el ends here
