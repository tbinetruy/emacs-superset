;;; emacs-superset-worktree.el --- Worktree lifecycle management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Create, delete, list, and refresh git worktrees via magit.

;;; Code:

(require 'emacs-superset-core)
(require 'magit)

;;; Worktree creation

(defun emacs-superset-worktree--name-from-branch (branch)
  "Derive a workspace name from BRANCH by replacing slashes with dashes."
  (replace-regexp-in-string "/" "-" branch))

(defun emacs-superset-worktree--create-internal (branch base-branch new-branch-p)
  "Create a workspace for BRANCH based on BASE-BRANCH.
If NEW-BRANCH-P, create a new branch; otherwise checkout existing.
Returns the new workspace struct."
  (let* ((name (emacs-superset-worktree--name-from-branch branch))
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
    ;; Create worktree
    (let ((default-directory repo-root))
      (if new-branch-p
          (magit-run-git "worktree" "add" "-b" branch wt-path base-branch)
        (magit-run-git "worktree" "add" wt-path branch)))
    ;; Build workspace struct
    (let ((ws (emacs-superset-workspace-create
               :path wt-path
               :branch branch
               :name name
               :base-branch (or base-branch (magit-get-current-branch))
               :agent-type emacs-superset-default-agent
               :created-at (float-time))))
      (emacs-superset--register-workspace ws)
      ;; Install Claude Code hooks
      (when (fboundp 'emacs-superset-hooks-install)
        (emacs-superset-hooks-install ws))
      (when (fboundp 'emacs-superset-tab-create)
        (emacs-superset-tab-create ws))
      (when (fboundp 'emacs-superset-config-run-setup)
        (emacs-superset-config-run-setup ws))
      (when (fboundp 'emacs-superset-dashboard-redraw)
        (emacs-superset-dashboard-redraw))
      (when (fboundp 'emacs-superset-watch-start-workspace)
        (emacs-superset-watch-start-workspace ws))
      (emacs-superset-worktree-refresh-git-state-async
       ws
       (lambda (_workspace)
         (when (fboundp 'emacs-superset-dashboard-request-redraw)
           (emacs-superset-dashboard-request-redraw))))
      (message "Created workspace: %s (branch: %s)" name branch)
      ws)))

(defun emacs-superset-worktree-create (branch &optional base-branch)
  "Create a new workspace with a new BRANCH based on BASE-BRANCH.
Prompts for the base branch first, then the new branch name."
  (interactive
   (let ((base (magit-read-branch-or-commit "Base branch" "HEAD")))
     (list (read-string (format "New branch name (%s prefix): "
                                emacs-superset-branch-prefix)
                        emacs-superset-branch-prefix)
           base)))
  (emacs-superset-worktree--create-internal
   branch (or base-branch "HEAD") t))

(defun emacs-superset-worktree-create-from-branch (branch)
  "Create a workspace by checking out an existing BRANCH."
  (interactive
   (list (magit-read-branch "Branch")))
  (emacs-superset-worktree--create-internal branch nil nil))

;;; Worktree deletion

(defun emacs-superset-worktree-delete (workspace &optional _force)
  "Delete WORKSPACE worktree and its branch."
  (interactive
   (list (emacs-superset--read-workspace "Delete workspace: ")
         current-prefix-arg))
  (let* ((name (emacs-superset-workspace-name workspace))
         (path (emacs-superset-workspace-path workspace))
         (branch (emacs-superset-workspace-branch workspace))
         (dirty-p (not (zerop (emacs-superset-workspace-uncommitted workspace))))
         (prompt (if dirty-p
                     (format "Workspace %s has uncommitted changes. Delete anyway? " name)
                   (format "Delete workspace %s? " name))))
    (when (yes-or-no-p prompt)
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
      ;; Remove worktree (always force — user already confirmed)
      (let ((default-directory (emacs-superset--repo-root)))
        (magit-run-git "worktree" "remove" "--force" path))
      ;; Delete branch (best effort, use -D since worktree may not be merged)
      (condition-case nil
          (let ((default-directory (emacs-superset--repo-root)))
            (magit-run-git "branch" "-D" branch))
        (error nil))
      ;; Unregister
      (when (fboundp 'emacs-superset-watch-stop-workspace)
        (emacs-superset-watch-stop-workspace workspace))
      (emacs-superset--unregister-workspace workspace)
      (when (fboundp 'emacs-superset-dashboard-redraw)
        (emacs-superset-dashboard-redraw))
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
         (git-worktrees (emacs-superset-worktree--git-list repo-root))
         (removed nil)
         (added nil))
    ;; Remove workspaces whose paths no longer exist in git worktree list
    (dolist (ws (emacs-superset--all-workspaces))
      (unless (member (emacs-superset--normalize-path
                       (emacs-superset-workspace-path ws))
                      git-worktrees)
        (push ws removed)
        (emacs-superset--unregister-workspace ws)))
    ;; Add all worktrees we're not tracking (skip the main repo itself)
    (dolist (wt-path git-worktrees)
      (unless (or (equal (emacs-superset--normalize-path wt-path) repo-root)
                  (emacs-superset--get-workspace wt-path))
        (let* ((name (file-name-nondirectory (directory-file-name wt-path)))
               (branch (emacs-superset-worktree--branch-at wt-path)))
          (let ((workspace
                 (emacs-superset-workspace-create
                  :path wt-path
                  :branch branch
                  :name name
                  :agent-type emacs-superset-default-agent
                  :created-at (float-time))))
            (emacs-superset--register-workspace workspace)
            (push workspace added)))))
    (when (fboundp 'emacs-superset-watch-stop-workspace)
      (dolist (workspace removed)
        (emacs-superset-watch-stop-workspace workspace)))
    (dolist (workspace added)
      (when (fboundp 'emacs-superset-watch-start-workspace)
        (emacs-superset-watch-start-workspace workspace))
      (emacs-superset-worktree-refresh-git-state-async
       workspace
       (lambda (_workspace)
         (when (fboundp 'emacs-superset-dashboard-request-redraw)
           (emacs-superset-dashboard-request-redraw)))))))

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

(defvar emacs-superset-worktree--git-refresh-processes
  (make-hash-table :test 'equal)
  "In-flight async git refresh processes keyed by normalized workspace path.")

(defcustom emacs-superset-git-refresh-max-concurrent 2
  "Maximum number of concurrent async git state refresh processes."
  :type 'integer
  :group 'emacs-superset)

(defvar emacs-superset-worktree--git-refresh-queue nil
  "Queued async git refresh requests as (WORKSPACE CALLBACK) entries.")

(defvar emacs-superset-worktree--git-refresh-queued-paths
  (make-hash-table :test 'equal)
  "Workspace paths currently queued for async git refresh.")

(defvar emacs-superset-worktree--git-refresh-pending-paths
  (make-hash-table :test 'equal)
  "Workspace paths that need another refresh after the active one exits.")

(defvar emacs-superset-worktree--git-refresh-active-count 0
  "Number of currently running async git refresh processes.")

(defun emacs-superset-worktree-refresh-git-state (workspace)
  "Refresh git state (branch, uncommitted, ahead, behind) for WORKSPACE."
  (let ((default-directory (emacs-superset-workspace-path workspace)))
    ;; Current branch (may have changed)
    (setf (emacs-superset-workspace-branch workspace)
          (emacs-superset-worktree--branch-at default-directory))
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
  ;; Listening ports for the workspace's terminal process tree
  (setf (emacs-superset-workspace-ports workspace)
        (emacs-superset-worktree--listening-ports workspace))
  workspace)

(defun emacs-superset-worktree-refresh-git-state-async (workspace &optional callback)
  "Refresh cached git state for WORKSPACE asynchronously.
CALLBACK, when non-nil, is called with WORKSPACE after a successful refresh."
  (let* ((path (emacs-superset--normalize-path
                (emacs-superset-workspace-path workspace)))
         (existing (gethash path emacs-superset-worktree--git-refresh-processes)))
    (if (and existing (process-live-p existing))
        (puthash path (list workspace callback)
                 emacs-superset-worktree--git-refresh-pending-paths)
      (if (and emacs-superset-git-refresh-max-concurrent
               (>= emacs-superset-worktree--git-refresh-active-count
                   emacs-superset-git-refresh-max-concurrent))
          (unless (gethash path emacs-superset-worktree--git-refresh-queued-paths)
            (puthash path t emacs-superset-worktree--git-refresh-queued-paths)
            (setq emacs-superset-worktree--git-refresh-queue
                  (append emacs-superset-worktree--git-refresh-queue
                          (list (list workspace callback)))))
        (emacs-superset-worktree--start-git-state-refresh workspace callback)))))

(defun emacs-superset-worktree--start-git-state-refresh (workspace callback)
  "Start an async git state refresh for WORKSPACE with CALLBACK."
  (let* ((path (emacs-superset--normalize-path
                (emacs-superset-workspace-path workspace)))
         (buffer (generate-new-buffer
                  (format " *emacs-superset-git:%s*"
                          (emacs-superset-workspace-name workspace))))
         (default-directory (file-name-as-directory path))
         (process
          (make-process
           :name (format "emacs-superset-git:%s"
                         (emacs-superset-workspace-name workspace))
           :buffer buffer
           :noquery t
           :connection-type 'pipe
           :command
           (list shell-file-name shell-command-switch
                 (concat
                  "printf '__branch__\\n'; "
                  "git rev-parse --abbrev-ref HEAD 2>/dev/null; "
                  "printf '__uncommitted__\\n'; "
                  "git --no-optional-locks status --porcelain "
                  "--untracked-files=normal 2>/dev/null | sed '/^$/d' | wc -l; "
                  "printf '__ahead_behind__\\n'; "
                  "git rev-list --left-right --count HEAD...@{upstream} 2>/dev/null "
                  "|| printf '0\\t0\\n'"))
           :sentinel #'emacs-superset-worktree--git-refresh-sentinel)))
    (cl-incf emacs-superset-worktree--git-refresh-active-count)
    (process-put process 'emacs-superset-workspace-path path)
    (process-put process 'emacs-superset-workspace workspace)
    (process-put process 'emacs-superset-callback callback)
    (puthash path process emacs-superset-worktree--git-refresh-processes)
    process))

(defun emacs-superset-worktree--git-refresh-sentinel (process _event)
  "Apply async git refresh results from PROCESS."
  (unless (process-live-p process)
    (let* ((path (process-get process 'emacs-superset-workspace-path))
           (workspace (process-get process 'emacs-superset-workspace))
           (callback (process-get process 'emacs-superset-callback))
           (buffer (process-buffer process)))
      (remhash path emacs-superset-worktree--git-refresh-processes)
      (unwind-protect
          (when (and buffer
                     (buffer-live-p buffer)
                     (zerop (process-exit-status process))
                     (eq workspace (emacs-superset--get-workspace path)))
            (with-current-buffer buffer
              (emacs-superset-worktree--apply-git-state-output
               workspace
               (buffer-string)))
            (when callback
              (funcall callback workspace)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (setq emacs-superset-worktree--git-refresh-active-count
              (max 0 (1- emacs-superset-worktree--git-refresh-active-count)))
        (when-let ((pending (gethash
                             path
                             emacs-superset-worktree--git-refresh-pending-paths)))
          (remhash path emacs-superset-worktree--git-refresh-pending-paths)
          (pcase-let ((`(,pending-workspace ,pending-callback) pending))
            (when (eq pending-workspace (emacs-superset--get-workspace path))
              (emacs-superset-worktree-refresh-git-state-async
               pending-workspace pending-callback))))
        (emacs-superset-worktree--pump-git-refresh-queue)))))

(defun emacs-superset-worktree--pump-git-refresh-queue ()
  "Start queued async git refreshes up to the concurrency limit."
  (while (and emacs-superset-worktree--git-refresh-queue
              (or (null emacs-superset-git-refresh-max-concurrent)
                  (< emacs-superset-worktree--git-refresh-active-count
                     emacs-superset-git-refresh-max-concurrent)))
    (pcase-let* ((`(,workspace ,callback)
                  (pop emacs-superset-worktree--git-refresh-queue))
                 (path (emacs-superset--normalize-path
                        (emacs-superset-workspace-path workspace))))
      (remhash path emacs-superset-worktree--git-refresh-queued-paths)
      (when (and (eq workspace (emacs-superset--get-workspace path))
                 (not (let ((process (gethash
                                       path
                                       emacs-superset-worktree--git-refresh-processes)))
                        (and process (process-live-p process)))))
        (emacs-superset-worktree--start-git-state-refresh workspace callback)))))

(defun emacs-superset-worktree--apply-git-state-output (workspace output)
  "Apply parsed async git state OUTPUT to WORKSPACE."
  (let ((branch nil)
        (uncommitted nil)
        (ahead 0)
        (behind 0)
        (section nil))
    (dolist (line (split-string output "\n" t))
      (cond
       ((equal line "__branch__")
        (setq section 'branch))
       ((equal line "__uncommitted__")
        (setq section 'uncommitted))
       ((equal line "__ahead_behind__")
        (setq section 'ahead-behind))
       ((eq section 'branch)
        (setq branch (string-trim line)
              section nil))
       ((eq section 'uncommitted)
        (setq uncommitted (string-to-number (string-trim line))
              section nil))
       ((eq section 'ahead-behind)
        (when (string-match "\\([0-9]+\\)[[:space:]]+\\([0-9]+\\)" line)
          (setq ahead (string-to-number (match-string 1 line))
                behind (string-to-number (match-string 2 line))))
        (setq section nil))))
    (when branch
      (setf (emacs-superset-workspace-branch workspace) branch))
    (when uncommitted
      (setf (emacs-superset-workspace-uncommitted workspace) uncommitted))
    (setf (emacs-superset-workspace-ahead workspace) ahead)
    (setf (emacs-superset-workspace-behind workspace) behind)
    workspace))

(defun emacs-superset-worktree-refresh-all-git-state-async (&optional callback)
  "Refresh cached git state for all tracked workspaces asynchronously.
CALLBACK is passed to each individual workspace refresh."
  (dolist (workspace (emacs-superset--all-workspaces))
    (emacs-superset-worktree-refresh-git-state-async workspace callback)))

(defun emacs-superset-worktree--listening-ports (workspace)
  "Return a list of listening TCP port numbers for WORKSPACE's process trees.
Scans all terminal buffers (shell terminals + agent) for the workspace."
  (condition-case nil
      (let* ((name (emacs-superset-workspace-name workspace))
             (term-bufs (emacs-superset--workspace-terminal-buffers name))
             (agent-buf (emacs-superset-workspace-agent-buffer workspace))
             (all-bufs (if (and agent-buf (buffer-live-p agent-buf))
                           (cons agent-buf term-bufs)
                         term-bufs))
             (all-ports nil))
        (dolist (buf all-bufs)
          (when-let ((proc (get-buffer-process buf))
                     (pid (process-id proc))
                     (ports (emacs-superset-worktree--ports-for-pid-tree pid)))
            (setq all-ports (append all-ports ports))))
        (sort (delete-dups all-ports) #'<))
    (error nil)))

(defun emacs-superset-worktree--ports-for-pid-tree (pid)
  "Return listening TCP ports for PID and all its descendants."
  (let ((output (string-trim
                 (with-temp-buffer
                   (call-process
                    shell-file-name nil t nil shell-command-switch
                    (format "
get_pids() {
  echo $1
  for child in $(pgrep -P $1 2>/dev/null); do
    get_pids $child
  done
}
PIDS=$(get_pids %d | tr '\\n' '|' | sed 's/|$//')
if [ -n \"$PIDS\" ]; then
  for p in $(echo $PIDS | tr '|' ' '); do
    ls -la /proc/$p/fd 2>/dev/null
  done | grep socket | sed 's/.*socket:\\[\\([0-9]*\\)\\]/\\1/' | sort -u | while read inode; do
    awk -v ino=\"$inode\" '$4==\"0A\" && $10==ino {split($2,a,\":\"); printf \"%%d\\n\", strtonum(\"0x\" a[2])}' /proc/net/tcp
  done
fi
" pid))
                   (buffer-string)))))
    (when (not (string-empty-p output))
      (sort (mapcar #'string-to-number (split-string output "\n" t))
            #'<))))

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
