;;; emacs-superset-config.el --- Project configuration loading  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Load project-specific configuration for workspace setup/teardown.
;;
;; Config lookup order (first found wins):
;;   1. .superset/config.local.json  (personal overrides, gitignored)
;;   2. .superset/config.json        (project default, committed)
;;   3. .emacs-superset.el           (Emacs-native alist format)
;;   4. .dir-locals.el               (Emacs-native, emacs-superset-* vars)
;;
;; The .superset/config.json format is compatible with superset.sh:
;;   { "setup": ["cmd1", "cmd2"], "teardown": ["cmd3"], "run": ["cmd4"] }

;;; Code:

(require 'emacs-superset-core)
(require 'json)

;;; Configuration variables (settable in .dir-locals.el)

(defvar-local emacs-superset-setup-commands nil
  "List of shell commands to run when setting up a workspace.
Can be set in .dir-locals.el or .emacs-superset.el.")

(defvar-local emacs-superset-teardown-commands nil
  "List of shell commands to run when tearing down a workspace.
Can be set in .dir-locals.el or .emacs-superset.el.")

(defvar-local emacs-superset-agent-extra-args nil
  "Extra arguments to pass to the agent command.
Can be set in .dir-locals.el or .emacs-superset.el.")

;; Mark as safe for .dir-locals.el (string lists only)
(dolist (var '(emacs-superset-setup-commands
               emacs-superset-teardown-commands
               emacs-superset-agent-extra-args))
  (put var 'safe-local-variable #'emacs-superset-config--safe-string-list-p))

(defun emacs-superset-config--safe-string-list-p (val)
  "Return non-nil if VAL is a list of strings or nil."
  (or (null val)
      (and (listp val) (cl-every #'stringp val))))

;;; Config loading

(defun emacs-superset-config-load (project-root)
  "Load workspace configuration for PROJECT-ROOT.
Checks sources in order: .superset/config.local.json,
.superset/config.json, .emacs-superset.el, .dir-locals.el.
Returns an alist with keys: setup, teardown, run."
  (let ((repo-root (condition-case nil
                       (emacs-superset--repo-root project-root)
                     (error project-root))))
    (or (emacs-superset-config--load-superset-json
         (expand-file-name ".superset/config.local.json" repo-root))
        (emacs-superset-config--load-superset-json
         (expand-file-name ".superset/config.json" repo-root))
        (emacs-superset-config--load-superset-json
         (expand-file-name ".superset/config.local.json" project-root))
        (emacs-superset-config--load-superset-json
         (expand-file-name ".superset/config.json" project-root))
        (emacs-superset-config--load-elisp
         (expand-file-name ".emacs-superset.el" project-root))
        (emacs-superset-config--from-dir-locals project-root)
        ;; No config found
        '((setup) (teardown) (run)))))

;;; .superset/config.json loader (superset.sh compatible)

(defun emacs-superset-config--load-superset-json (file)
  "Load superset config from JSON FILE.
Returns an alist with keys setup, teardown, run, or nil if FILE
does not exist."
  (when (file-exists-p file)
    (condition-case err
        (let* ((json-array-type 'list)
               (json-key-type 'symbol)
               (data (json-read-file file)))
          `((setup . ,(alist-get 'setup data))
            (teardown . ,(alist-get 'teardown data))
            (run . ,(alist-get 'run data))))
      (error
       (message "emacs-superset: Error loading %s: %s" file err)
       nil))))

;;; .emacs-superset.el loader

(defun emacs-superset-config--load-elisp (file)
  "Load emacs-superset config from elisp FILE.
Returns an alist, or nil if FILE does not exist."
  (when (file-exists-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (unless (listp data)
              (error "Config file should contain an alist"))
            `((setup . ,(alist-get 'setup data))
              (teardown . ,(alist-get 'teardown data))
              (run . ,(alist-get 'run data)))))
      (error
       (message "emacs-superset: Error loading %s: %s" file err)
       nil))))

;;; .dir-locals.el loader

(defun emacs-superset-config--from-dir-locals (project-root)
  "Extract emacs-superset config from .dir-locals.el in PROJECT-ROOT.
Returns an alist, or nil if no emacs-superset vars are set."
  (let ((setup nil)
        (teardown nil))
    (with-temp-buffer
      (setq default-directory (file-name-as-directory project-root))
      (hack-dir-local-variables-non-file-buffer)
      (setq setup emacs-superset-setup-commands)
      (setq teardown emacs-superset-teardown-commands))
    (when (or setup teardown)
      `((setup . ,setup)
        (teardown . ,teardown)
        (run)))))

;;; Setup/teardown/run execution

(defun emacs-superset-config-run-setup (workspace)
  "Run setup commands for WORKSPACE asynchronously."
  (interactive (list (emacs-superset--read-workspace "Run setup for: ")))
  (let* ((path (emacs-superset-workspace-path workspace))
         (config (emacs-superset-config-load path))
         (commands (alist-get 'setup config)))
    (if commands
        (emacs-superset-config--run-commands
         workspace commands "setup"
         (lambda ()
           (setf (emacs-superset-workspace-setup-ran-p workspace) t)
           (message "Setup complete for %s"
                    (emacs-superset-workspace-name workspace))))
      (setf (emacs-superset-workspace-setup-ran-p workspace) t)
      (message "No setup commands for %s"
               (emacs-superset-workspace-name workspace)))))

(defun emacs-superset-config-run-teardown (workspace)
  "Run teardown commands for WORKSPACE asynchronously."
  (interactive (list (emacs-superset--read-workspace "Run teardown for: ")))
  (let* ((path (emacs-superset-workspace-path workspace))
         (config (emacs-superset-config-load path))
         (commands (alist-get 'teardown config)))
    (if commands
        (emacs-superset-config--run-commands
         workspace commands "teardown"
         (lambda ()
           (message "Teardown complete for %s"
                    (emacs-superset-workspace-name workspace))))
      (message "No teardown commands for %s"
               (emacs-superset-workspace-name workspace)))))

(defun emacs-superset-config-run-run (workspace)
  "Run the on-demand `run' commands for WORKSPACE."
  (interactive (list (emacs-superset--read-workspace "Run commands for: ")))
  (let* ((path (emacs-superset-workspace-path workspace))
         (config (emacs-superset-config-load path))
         (commands (alist-get 'run config)))
    (if commands
        (emacs-superset-config--run-commands
         workspace commands "run"
         (lambda ()
           (message "Run complete for %s"
                    (emacs-superset-workspace-name workspace))))
      (message "No run commands for %s"
               (emacs-superset-workspace-name workspace)))))

(defun emacs-superset-config--run-commands (workspace commands phase callback)
  "Run COMMANDS sequentially for WORKSPACE during PHASE.
CALLBACK is called when all commands complete successfully."
  (let* ((name (emacs-superset-workspace-name workspace))
         (path (emacs-superset-workspace-path workspace))
         (repo-root (condition-case nil
                        (emacs-superset--repo-root path)
                      (error path))))
    (emacs-superset-config--run-next commands path repo-root name phase callback)))

(defun emacs-superset-config--run-next (commands path repo-root name phase callback)
  "Run the next command in COMMANDS for workspace at PATH.
REPO-ROOT, NAME and PHASE are for env vars and logging.
CALLBACK when all done."
  (if (null commands)
      (when callback (funcall callback))
    (let* ((cmd (car commands))
           (rest (cdr commands))
           (buf-name (format "*superset:%s:%s*" name phase))
           (default-directory (file-name-as-directory path))
           (process-environment
            (append (list (format "SUPERSET_ROOT_PATH=%s" repo-root)
                          (format "SUPERSET_WORKSPACE_NAME=%s" name)
                          (format "SUPERSET_WORKSPACE_PATH=%s" path)
                          ;; Also set our own prefixed variants
                          (format "EMACS_SUPERSET_ROOT=%s" repo-root)
                          (format "EMACS_SUPERSET_WORKSPACE=%s" name)
                          (format "EMACS_SUPERSET_PATH=%s" path))
                    process-environment))
           (proc (start-process-shell-command buf-name buf-name cmd)))
      (message "emacs-superset [%s/%s]: %s" name phase cmd)
      (set-process-sentinel
       proc
       (lambda (process event)
         (if (string-match-p "finished" event)
             (emacs-superset-config--run-next
              rest path repo-root name phase callback)
           (message "emacs-superset [%s/%s]: command failed: %s"
                    name phase (string-trim event))))))))

(provide 'emacs-superset-config)
;;; emacs-superset-config.el ends here
