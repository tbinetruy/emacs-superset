;;; emacs-superset-test.el --- Tests for emacs-superset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;;; Commentary:

;; ERT tests for the emacs-superset package.
;; Run with: emacs --batch -L lisp/ -L test/ -l ert -l emacs-superset-test -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-superset-core)

;; Stub out external dependencies for unit tests
(unless (featurep 'magit-section)
  (provide 'magit-section)
  (defvar magit-section-mode-map (make-sparse-keymap))
  (defun magit-section-mode ())
  (defmacro magit-insert-section (_args &rest body) `(progn ,@body))
  (defmacro magit-insert-heading (&rest _args))
  (defun magit-current-section () nil))

(unless (featurep 'magit)
  (provide 'magit)
  (defun magit-run-git (&rest _args))
  (defun magit-read-branch (&rest _args) "main")
  (defun magit-read-branch-or-commit (&rest _args) "main")
  (defun magit-get-current-branch () "main")
  (defun magit-list-worktrees () nil)
  (defun magit-diff-range (&rest _args))
  (defun magit-diff-unstaged (&rest _args))
  (defun magit-diff-staged (&rest _args))
  (defun magit-status-setup-buffer (&rest _args)))

(unless (featurep 'eat)
  (provide 'eat)
  (defun eat-mode ())
  (defun eat-exec (&rest _args)))

(unless (featurep 'vterm)
  (provide 'vterm)
  (defun vterm (&optional _name)))

(require 'emacs-superset-worktree)
(require 'emacs-superset-config)
(require 'emacs-superset-dashboard)

;;; ---- Core: workspace struct ----

(ert-deftest emacs-superset-test-workspace-create ()
  "Workspace struct has correct defaults."
  (let ((ws (emacs-superset-workspace-create
             :path "/tmp/test-wt"
             :branch "superset/test"
             :name "test")))
    (should (equal (emacs-superset-workspace-path ws) "/tmp/test-wt"))
    (should (equal (emacs-superset-workspace-branch ws) "superset/test"))
    (should (equal (emacs-superset-workspace-name ws) "test"))
    (should (eq (emacs-superset-workspace-agent-status ws) 'idle))
    (should (= (emacs-superset-workspace-uncommitted ws) 0))
    (should (= (emacs-superset-workspace-ahead ws) 0))
    (should (= (emacs-superset-workspace-behind ws) 0))
    (should (null (emacs-superset-workspace-setup-ran-p ws)))))

;;; ---- Core: hash table operations ----

(ert-deftest emacs-superset-test-register-and-get ()
  "Register a workspace and retrieve it by path."
  (let ((emacs-superset--workspaces (make-hash-table :test 'equal))
        (ws (emacs-superset-workspace-create
             :path "/tmp/wt1"
             :name "wt1")))
    (emacs-superset--register-workspace ws)
    (should (eq (emacs-superset--get-workspace "/tmp/wt1") ws))
    (should (null (emacs-superset--get-workspace "/tmp/nonexistent")))))

(ert-deftest emacs-superset-test-unregister ()
  "Unregistering removes workspace from lookup."
  (let ((emacs-superset--workspaces (make-hash-table :test 'equal))
        (ws (emacs-superset-workspace-create :path "/tmp/wt1" :name "wt1")))
    (emacs-superset--register-workspace ws)
    (should (emacs-superset--get-workspace "/tmp/wt1"))
    (emacs-superset--unregister-workspace ws)
    (should (null (emacs-superset--get-workspace "/tmp/wt1")))))

(ert-deftest emacs-superset-test-all-workspaces-sorted ()
  "All workspaces are returned sorted by name."
  (let ((emacs-superset--workspaces (make-hash-table :test 'equal)))
    (emacs-superset--register-workspace
     (emacs-superset-workspace-create :path "/tmp/c" :name "charlie"))
    (emacs-superset--register-workspace
     (emacs-superset-workspace-create :path "/tmp/a" :name "alpha"))
    (emacs-superset--register-workspace
     (emacs-superset-workspace-create :path "/tmp/b" :name "bravo"))
    (let ((names (mapcar #'emacs-superset-workspace-name
                         (emacs-superset--all-workspaces))))
      (should (equal names '("alpha" "bravo" "charlie"))))))

(ert-deftest emacs-superset-test-path-normalization ()
  "Paths with trailing slashes resolve to the same workspace."
  (let ((emacs-superset--workspaces (make-hash-table :test 'equal))
        (ws (emacs-superset-workspace-create :path "/tmp/wt1/" :name "wt1")))
    (emacs-superset--register-workspace ws)
    ;; expand-file-name normalizes trailing slashes
    (should (eq (emacs-superset--get-workspace "/tmp/wt1/") ws))
    (should (eq (emacs-superset--get-workspace "/tmp/wt1") ws))))

;;; ---- Core: worktree base path computation ----

(ert-deftest emacs-superset-test-worktree-base-path ()
  "Worktree base path is computed correctly from repo root."
  (let ((emacs-superset-worktree-base-dir "worktrees"))
    (should (equal (emacs-superset--worktree-base-path "/home/user/code/myproject/")
                   "/home/user/code/worktrees/myproject"))))

(ert-deftest emacs-superset-test-worktree-base-path-custom-dir ()
  "Custom worktree-base-dir is respected."
  (let ((emacs-superset-worktree-base-dir "wt"))
    (should (equal (emacs-superset--worktree-base-path "/home/user/code/repo/")
                   "/home/user/code/wt/repo"))))

;;; ---- Config: file loading ----

(ert-deftest emacs-superset-test-config-load-file ()
  "Config file is parsed correctly."
  (let ((config-file (make-temp-file "superset-config" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "((setup . (\"npm install\" \"npm run build\"))\n"
                    " (teardown . (\"npm run stop\"))\n"
                    " (run . (\"./.superset/run.sh\")))"))
          (let ((config (emacs-superset-config--load-elisp config-file)))
            (should (equal (alist-get 'setup config)
                           '("npm install" "npm run build")))
            (should (equal (alist-get 'teardown config)
                           '("npm run stop")))
            (should (equal (alist-get 'run config)
                           '("./.superset/run.sh")))))
      (delete-file config-file))))

(ert-deftest emacs-superset-test-config-load-file-missing-keys ()
  "Config file with partial keys returns nil for missing ones."
  (let ((config-file (make-temp-file "superset-config" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "((setup . (\"make\")))"))
          (let ((config (emacs-superset-config--load-elisp config-file)))
            (should (equal (alist-get 'setup config) '("make")))
            (should (null (alist-get 'teardown config)))
            (should (null (alist-get 'agent-extra-args config)))))
      (delete-file config-file))))

(ert-deftest emacs-superset-test-config-load-file-invalid ()
  "Invalid config file returns nil without error."
  (let ((config-file (make-temp-file "superset-config" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "this is not valid elisp {{{{"))
          (should (null (emacs-superset-config--load-elisp config-file))))
      (delete-file config-file))))

;;; ---- Config: .superset/config.json loading ----

(ert-deftest emacs-superset-test-config-load-superset-json ()
  "Superset JSON config is parsed correctly."
  (let ((config-file (make-temp-file "superset-config" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "{\"setup\": [\"bun install\", \"cp .env.example .env\"],\n"
                    " \"teardown\": [\"docker-compose down\"],\n"
                    " \"run\": [\"./.superset/run.sh\"]}"))
          (let ((config (emacs-superset-config--load-superset-json config-file)))
            (should (equal (alist-get 'setup config)
                           '("bun install" "cp .env.example .env")))
            (should (equal (alist-get 'teardown config)
                           '("docker-compose down")))
            (should (equal (alist-get 'run config)
                           '("./.superset/run.sh")))))
      (delete-file config-file))))

(ert-deftest emacs-superset-test-config-load-superset-json-partial ()
  "Superset JSON config with only setup key works."
  (let ((config-file (make-temp-file "superset-config" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "{\"setup\": [\"make\"]}"))
          (let ((config (emacs-superset-config--load-superset-json config-file)))
            (should (equal (alist-get 'setup config) '("make")))
            (should (null (alist-get 'teardown config)))
            (should (null (alist-get 'run config)))))
      (delete-file config-file))))

(ert-deftest emacs-superset-test-config-load-superset-json-missing ()
  "Non-existent JSON file returns nil."
  (should (null (emacs-superset-config--load-superset-json "/tmp/nonexistent.json"))))

(ert-deftest emacs-superset-test-safe-string-list-p ()
  "Safe local variable predicate works correctly."
  (should (emacs-superset-config--safe-string-list-p nil))
  (should (emacs-superset-config--safe-string-list-p '("a" "b")))
  (should-not (emacs-superset-config--safe-string-list-p '(1 2)))
  (should-not (emacs-superset-config--safe-string-list-p "string"))
  (should-not (emacs-superset-config--safe-string-list-p '("ok" 42))))

;;; ---- Dashboard: status indicator formatting ----

(ert-deftest emacs-superset-test-status-indicator ()
  "Status indicators return correct text."
  (should (equal (substring-no-properties
                  (emacs-superset-dashboard--status-indicator 'idle))
                 "○ idle"))
  (should (equal (substring-no-properties
                  (emacs-superset-dashboard--status-indicator 'running))
                 "● running"))
  (should (equal (substring-no-properties
                  (emacs-superset-dashboard--status-indicator 'waiting))
                 "◌ waiting"))
  (should (equal (substring-no-properties
                  (emacs-superset-dashboard--status-indicator 'done))
                 "✓ done"))
  (should (equal (substring-no-properties
                  (emacs-superset-dashboard--status-indicator 'error))
                 "✗ error")))

;;; ---- Integration: worktree create/delete with real git ----

(ert-deftest emacs-superset-test-worktree-lifecycle ()
  "Create and delete a worktree in a temporary git repo."
  (let* ((tmpdir (make-temp-file "superset-test" t))
         (repo-dir (expand-file-name "repo" tmpdir))
         (emacs-superset--workspaces (make-hash-table :test 'equal))
         (emacs-superset-worktree-base-dir "worktrees")
         (emacs-superset-branch-prefix "superset/")
         (emacs-superset-default-agent 'claude-code))
    (unwind-protect
        (progn
          ;; Set up a minimal git repo with one commit
          (make-directory repo-dir t)
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init" "-b" "main")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README" repo-dir)
              (insert "test"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "init"))
          ;; Create a workspace (bypass interactive prompts and tab creation)
          (let* ((name "test-feature")
                 (branch (concat emacs-superset-branch-prefix name))
                 (wt-base (emacs-superset--worktree-base-path repo-dir))
                 (wt-path (expand-file-name name wt-base))
                 (default-directory repo-dir))
            (make-directory (file-name-directory wt-path) t)
            (call-process "git" nil nil nil
                          "worktree" "add" "-b" branch wt-path "main")
            ;; Register it
            (let ((ws (emacs-superset-workspace-create
                       :path wt-path
                       :branch branch
                       :name name
                       :base-branch "main"
                       :created-at (float-time))))
              (emacs-superset--register-workspace ws)
              ;; Verify worktree exists
              (should (file-directory-p wt-path))
              (should (file-exists-p (expand-file-name "README" wt-path)))
              (should (emacs-superset--get-workspace wt-path))
              ;; Verify git state refresh works
              (emacs-superset-worktree-refresh-git-state ws)
              (should (= (emacs-superset-workspace-uncommitted ws) 0))
              ;; Create a file to test uncommitted count
              (with-temp-file (expand-file-name "new-file" wt-path)
                (insert "new"))
              (emacs-superset-worktree-refresh-git-state ws)
              (should (= (emacs-superset-workspace-uncommitted ws) 1))
              ;; Clean up: remove worktree
              (let ((default-directory repo-dir))
                (call-process "git" nil nil nil "worktree" "remove" "--force" wt-path))
              (emacs-superset--unregister-workspace ws)
              (should (null (emacs-superset--get-workspace wt-path)))
              (should-not (file-directory-p wt-path)))))
      ;; Cleanup temp directory
      (delete-directory tmpdir t))))

;;; ---- Integration: repo-root detection ----

(ert-deftest emacs-superset-test-repo-root-detection ()
  "Repo root is detected correctly from within a worktree."
  (let ((tmpdir (make-temp-file "superset-test" t)))
    (unwind-protect
        (let* ((repo-dir (expand-file-name "repo" tmpdir))
               (wt-dir (expand-file-name "wt" tmpdir)))
          (make-directory repo-dir t)
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init" "-b" "main")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "f" repo-dir) (insert "x"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "init")
            (call-process "git" nil nil nil "worktree" "add" "-b" "feat" wt-dir "main"))
          ;; From the main repo
          (should (equal (file-name-as-directory repo-dir)
                         (emacs-superset--repo-root repo-dir)))
          ;; From the worktree — should still return main repo root
          (should (equal (file-name-as-directory repo-dir)
                         (emacs-superset--repo-root wt-dir)))
          ;; worktree-root should return the worktree itself
          (should (equal (expand-file-name wt-dir)
                         (emacs-superset--worktree-root wt-dir)))
          ;; Clean up
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "worktree" "remove" "--force" wt-dir)))
      (delete-directory tmpdir t))))

(provide 'emacs-superset-test)
;;; emacs-superset-test.el ends here
