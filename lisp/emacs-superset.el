;;; emacs-superset.el --- Agent orchestrator for parallel AI coding workflows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; Author: Thomas
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (transient "0.7.0"))
;; Keywords: tools, processes, vc
;; URL: https://github.com/thomas/emacs-superset

;; This file is part of emacs-superset.

;; emacs-superset is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; emacs-superset is an agent orchestrator for developers, inspired by
;; superset.sh.  It composes magit, transient, eat, and tab-bar-mode to
;; manage multiple AI coding agents running in parallel on the same codebase
;; via git worktrees.
;;
;; Core workflow:
;; 1. Create a workspace (git worktree + tab + terminal)
;; 2. Launch an agent (Claude Code, Gemini CLI, aider, etc.)
;; 3. Monitor all workspaces from the dashboard
;; 4. Review diffs via magit
;;
;; Entry point: M-x emacs-superset

;;; Code:

(require 'emacs-superset-core)
(require 'emacs-superset-worktree)
(require 'emacs-superset-tab)
(require 'emacs-superset-agent)
(require 'emacs-superset-dashboard)
(require 'emacs-superset-transient)
(require 'emacs-superset-diff)
(require 'emacs-superset-config)
(require 'emacs-superset-hooks)

(defconst emacs-superset-version "0.1.0"
  "Version of emacs-superset.")

;;;###autoload
(defalias 'emacs-superset #'emacs-superset-dispatch
  "Main entry point for emacs-superset.
Opens the transient menu for workspace and agent management.")

(provide 'emacs-superset)
;;; emacs-superset.el ends here
