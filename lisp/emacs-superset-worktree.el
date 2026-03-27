;;; emacs-superset-worktree.el --- Worktree lifecycle management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Create, delete, list, and refresh git worktrees via magit.

;;; Code:

(require 'emacs-superset-core)
(require 'magit)

;;; Worktree creation

(defun emacs-superset-worktree-create (name &optional base-branch)
  "Create a new workspace worktree named NAME.
BASE-BRANCH is the branch to base the worktree on (defaults to HEAD).
Returns the new workspace struct."
  (interactive
   (list (read-string "Workspace name: ")
         (magit-read-branch-or-commit "Base branch" "HEAD")))
  (let* ((repo-root (emacs-superset--repo-root))
         (base (or base-branch "HEAD"))
         (branch (concat emacs-superset-branch-prefix name))
         (wt-base (emacs-superset--worktree-base-path repo-root))
         (wt-path (expand-file-name name wt-base)))
    ;; Validate
    (when (emacs-superset--get-workspace wt-path)
      (user-error "Workspace %s already exists" name))
    (when (file-exists-p wt-path)
      (user-error "Directory %s already exists" wt-path))
    ;; Create parent directory if needed
    (make-directory (file-name-directory wt-path) t)
    ;; Create worktree with new branch
    (let ((default-directory repo-root))
      (magit-run-git "worktree" "add" "-b" branch wt-path base))
    ;; Build workspace struct
    (let ((ws (emacs-superset-workspace-create
               :path wt-path
               :branch branch
               :name name
               :base-branch (if (string= base "HEAD")
                                (magit-get-current-branch)
                              base)
               :agent-type emacs-superset-default-agent
               :created-at (float-time))))
      (emacs-superset--register-workspace ws)
      ;; Create tab for the workspace
      (when (fboundp 'emacs-superset-tab-create)
        (emacs-superset-tab-create ws))
      ;; Run setup hooks if configured
      (when (fboundp 'emacs-superset-config-run-setup)
        (emacs-superset-config-run-setup ws))
      (message "Created workspace: %s (branch: %s)" name branch)
      ws)))

(defun emacs-superset-worktree-create-from-branch (branch)
  "Create a workspace by checking out an existing BRANCH.
The workspace name is derived from the branch name with slashes
replaced by dashes."
  (interactive
   (list (magit-read-branch "Branch")))
  (let* ((name (replace-regexp-in-string "/" "-" branch))
         (repo-root (emacs-superset--repo-root))
         (wt-base (emacs-superset--worktree-base-path repo-root))
         (wt-path (expand-file-name name wt-base)))
    ;; Validate
    (when (emacs-superset--get-workspace wt-path)
      (user-error "Workspace %s already exists" name))
    (when (file-exists-p wt-path)
      (user-error "Directory %s already exists" wt-path))
    ;; Create parent directory if needed
    (make-directory (file-name-directory wt-path) t)
    ;; Checkout existing branch into worktree (no -b flag)
    (let ((default-directory repo-root))
      (magit-run-git "worktree" "add" wt-path branch))
    ;; Build workspace struct
    (let ((ws (emacs-superset-workspace-create
               :path wt-path
               :branch branch
               :name name
               :base-branch (magit-get-current-branch)
               :agent-type emacs-superset-default-agent
               :created-at (float-time))))
      (emacs-superset--register-workspace ws)
      (when (fboundp 'emacs-superset-tab-create)
        (emacs-superset-tab-create ws))
      (when (fboundp 'emacs-superset-config-run-setup)
        (emacs-superset-config-run-setup ws))
      (message "Created workspace: %s (branch: %s)" name branch)
      ws)))

;;; Worktree deletion

(defun emacs-superset-worktree-delete (workspace &optional force)
  "Delete WORKSPACE worktree and optionally its branch.
With prefix arg FORCE, force removal even with uncommitted changes."
  (interactive
   (list (emacs-superset--read-workspace "Delete workspace: ")
         current-prefix-arg))
  (let ((name (emacs-superset-workspace-name workspace))
        (path (emacs-superset-workspace-path workspace))
        (branch (emacs-superset-workspace-branch workspace)))
    (when (yes-or-no-p (format "Delete workspace %s? " name))
      ;; Run teardown hooks if configured
      (when (fboundp 'emacs-superset-config-run-teardown)
        (emacs-superset-config-run-teardown workspace))
      ;; Stop agent if running
      (when (and (eq (emacs-superset-workspace-agent-status workspace) 'running)
                 (fboundp 'emacs-superset-agent-stop))
        (emacs-superset-agent-stop workspace))
      ;; Kill agent buffer if it exists
      (when-let ((buf (emacs-superset-workspace-agent-buffer workspace)))
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      ;; Close tab
      (when (fboundp 'emacs-superset-tab-close)
        (emacs-superset-tab-close workspace))
      ;; Remove worktree
      (let ((default-directory (emacs-superset--repo-root)))
        (if force
            (magit-run-git "worktree" "remove" "--force" path)
          (magit-run-git "worktree" "remove" path)))
      ;; Delete branch (best effort)
      (condition-case nil
          (let ((default-directory (emacs-superset--repo-root)))
            (magit-run-git "branch" "-d" branch))
        (error nil))
      ;; Unregister
      (emacs-superset--unregister-workspace workspace)
      (message "Deleted workspace: %s" name))))

;;; Worktree listing

(defun emacs-superset-worktree-list ()
  "Return list of all workspace structs, reconciling with git."
  (emacs-superset-worktree-reconcile)
  (emacs-superset--all-workspaces))

(defun emacs-superset-worktree-reconcile ()
  "Reconcile tracked workspaces with what git reports.
Discovers all worktrees (excluding the main repo itself), removes
tracked workspaces whose worktrees no longer exist on disk."
  (let* ((repo-root (emacs-superset--normalize-path (emacs-superset--repo-root)))
         (git-worktrees (emacs-superset-worktree--git-list repo-root)))
    ;; Remove workspaces whose paths no longer exist in git worktree list
    (dolist (ws (emacs-superset--all-workspaces))
      (unless (member (emacs-superset--normalize-path
                       (emacs-superset-workspace-path ws))
                      git-worktrees)
        (emacs-superset--unregister-workspace ws)))
    ;; Add all worktrees we're not tracking (skip the main repo itself)
    (dolist (wt-path git-worktrees)
      (unless (or (equal (emacs-superset--normalize-path wt-path) repo-root)
                  (emacs-superset--get-workspace wt-path))
        (let* ((name (file-name-nondirectory (directory-file-name wt-path)))
               (branch (emacs-superset-worktree--branch-at wt-path)))
          (emacs-superset--register-workspace
           (emacs-superset-workspace-create
            :path wt-path
            :branch branch
            :name name
            :agent-type emacs-superset-default-agent
            :created-at (float-time))))))))

(defun emacs-superset-worktree--git-list (repo-root)
  "Return list of worktree paths reported by git in REPO-ROOT."
  (let ((default-directory repo-root))
    (mapcar (lambda (line)
              (expand-file-name (car (split-string line))))
            (seq-filter (lambda (s) (not (string-empty-p s)))
                        (split-string
                         (with-temp-buffer
                           (process-file "git" nil t nil
                                         "worktree" "list" "--porcelain")
                           (buffer-string))
                         "worktree " t)))))

(defun emacs-superset-worktree--branch-at (wt-path)
  "Return the branch name checked out at WT-PATH."
  (let ((default-directory wt-path))
    (string-trim
     (with-temp-buffer
       (process-file "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD")
       (buffer-string)))))

;;; Git state refresh

(defun emacs-superset-worktree-refresh-git-state (workspace)
  "Refresh git state (uncommitted, ahead, behind) for WORKSPACE."
  (let ((default-directory (emacs-superset-workspace-path workspace)))
    ;; Uncommitted changes count
    (setf (emacs-superset-workspace-uncommitted workspace)
          (length (seq-filter
                   (lambda (s) (not (string-empty-p s)))
                   (split-string
                    (with-temp-buffer
                      (process-file "git" nil t nil
                                    "status" "--porcelain" "--untracked-files=normal")
                      (buffer-string))
                    "\n" t))))
    ;; Ahead/behind upstream
    (let ((counts (string-trim
                   (with-temp-buffer
                     (process-file "git" nil t nil
                                   "rev-list" "--left-right" "--count"
                                   "HEAD...@{upstream}")
                     (buffer-string)))))
      (if (string-match "\\([0-9]+\\)\t\\([0-9]+\\)" counts)
          (progn
            (setf (emacs-superset-workspace-ahead workspace)
                  (string-to-number (match-string 1 counts)))
            (setf (emacs-superset-workspace-behind workspace)
                  (string-to-number (match-string 2 counts))))
        ;; No upstream configured
        (setf (emacs-superset-workspace-ahead workspace) 0)
        (setf (emacs-superset-workspace-behind workspace) 0))))
  workspace)

;;; Session restore

(defun emacs-superset-restore ()
  "Rediscover workspaces from the current git repo's worktrees."
  (interactive)
  (emacs-superset-worktree-reconcile)
  (let ((count (length (emacs-superset--all-workspaces))))
    (message "emacs-superset: Found %d workspace(s)" count)))

(defun emacs-superset-restore-tabs ()
  "Recreate tabs for all known workspaces that don't have one."
  (interactive)
  (dolist (ws (emacs-superset--all-workspaces))
    (unless (emacs-superset-workspace-tab-name ws)
      (when (fboundp 'emacs-superset-tab-create)
        (emacs-superset-tab-create ws)))))

(provide 'emacs-superset-worktree)
;;; emacs-superset-worktree.el ends here
