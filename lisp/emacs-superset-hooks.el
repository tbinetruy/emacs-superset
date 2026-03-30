;;; emacs-superset-hooks.el --- Claude Code hooks integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Integrates with Claude Code's hooks system to detect agent state
;; changes in real time.  When a claude-code agent is launched, we
;; install a hook script and register it for the following events:
;;
;;   - UserPromptSubmit: user sends a message     -> running
;;   - PreToolUse:       Claude invokes a tool     -> running
;;   - Stop:             Claude finishes a turn    -> done
;;   - StopFailure:      API error during turn     -> error
;;   - Notification:     needs permission/input    -> waiting
;;   - SessionEnd:       session terminates        -> idle
;;
;; The hook script calls emacsclient --eval to update workspace status.
;; Requires (server-start) in Emacs.

;;; Code:

(require 'emacs-superset-core)
(require 'json)

;;; Hook script

(defconst emacs-superset-hooks--script-name "emacs-superset-hook.sh"
  "Name of the hook script installed in each workspace.")

(defconst emacs-superset-hooks--events
  '(UserPromptSubmit PreToolUse PostToolUse PermissionRequest
    Stop StopFailure Notification SessionEnd)
  "Claude Code hook events we register for.")

(defun emacs-superset-hooks--script-content (workspace-path)
  "Return the hook script content for workspace at WORKSPACE-PATH.
The script reads JSON from stdin, extracts the event type, and calls
emacsclient to update the workspace status."
  (let ((path-lit (prin1-to-string workspace-path)))
    ;; path-lit is an elisp string literal like "\"/home/.../ws\""
    ;; We embed it once as a shell variable and reference it in each case.
    (format "#!/bin/sh
# emacs-superset hook - auto-generated, do not edit
# Updates agent status in emacs-superset dashboard via emacsclient

EVENT=$(cat)
HOOK_EVENT=$(echo \"$EVENT\" | sed -n 's/.*\"hook_event_name\" *: *\"\\([^\"]*\\)\".*/\\1/p')

ec() { emacsclient --no-wait --eval \"$1\" >/dev/null 2>&1; }
WS='%s'

case \"$HOOK_EVENT\" in
  UserPromptSubmit|PreToolUse|PostToolUse)
    ec \"(emacs-superset-hooks--on-activity $WS)\"
    ;;
  PermissionRequest)
    ec \"(emacs-superset-hooks--on-notification $WS)\"
    ;;
  Stop)
    ec \"(emacs-superset-hooks--on-stop $WS)\"
    ;;
  StopFailure)
    ec \"(emacs-superset-hooks--on-error $WS)\"
    ;;
  Notification)
    ec \"(emacs-superset-hooks--on-notification $WS)\"
    ;;
  SessionEnd)
    ec \"(emacs-superset-hooks--on-session-end $WS)\"
    ;;
esac

exit 0
" path-lit)))

;;; Hook installation

(defun emacs-superset-hooks--write-settings (settings-path script-path)
  "Merge hook SCRIPT-PATH into SETTINGS-PATH json file."
  (let* ((json-object-type 'alist)
         (json-key-type 'symbol)
         (settings (if (file-exists-p settings-path)
                       (condition-case nil
                           (json-read-file settings-path)
                         (error nil))
                     nil)))
    (setq settings (emacs-superset-hooks--merge-settings settings script-path))
    (with-temp-file settings-path
      (insert (json-encode settings)))))

(defun emacs-superset-hooks-install (workspace)
  "Install Claude Code hooks for WORKSPACE.
Creates a hook script and registers it in .claude/settings.local.json
in both the worktree and the main repo (since it's unclear which one
Claude Code reads when running inside a worktree)."
  (condition-case err
      (let* ((path (emacs-superset-workspace-path workspace))
             (repo-root (condition-case nil
                            (emacs-superset--repo-root path)
                          (error path)))
             (wt-claude-dir (expand-file-name ".claude" path))
             (script-path (expand-file-name emacs-superset-hooks--script-name wt-claude-dir))
             (repo-claude-dir (expand-file-name ".claude" repo-root)))
        ;; Create dirs
        (make-directory wt-claude-dir t)
        (make-directory repo-claude-dir t)
        ;; Write hook script in worktree
        (with-temp-file script-path
          (insert (emacs-superset-hooks--script-content path)))
        (set-file-modes script-path #o755)
        ;; Register in both worktree and main repo settings
        (emacs-superset-hooks--write-settings
         (expand-file-name "settings.local.json" wt-claude-dir) script-path)
        (emacs-superset-hooks--write-settings
         (expand-file-name "settings.local.json" repo-claude-dir) script-path)
        (message "emacs-superset: Installed hooks for %s"
                 (emacs-superset-workspace-name workspace)))
    (error
     (message "emacs-superset: Failed to install hooks: %s" err))))

(defun emacs-superset-hooks--merge-settings (settings script-path)
  "Merge emacs-superset hook entries into SETTINGS using SCRIPT-PATH."
  (let* ((settings (or settings '()))
         (existing-hooks (alist-get 'hooks settings))
         (hook-entry `((matcher . "")
                       (hooks . [((type . "command")
                                  (command . ,script-path))]))))
    ;; Add our hook to each event type if not already present
    (dolist (event emacs-superset-hooks--events)
      (let ((event-hooks (alist-get event existing-hooks)))
        (unless (emacs-superset-hooks--already-installed-p event-hooks script-path)
          (setq event-hooks
                (vconcat (or event-hooks []) (vector hook-entry))))
        (setf (alist-get event existing-hooks) event-hooks)))
    (setf (alist-get 'hooks settings) existing-hooks)
    settings))

(defun emacs-superset-hooks--already-installed-p (hook-entries script-path)
  "Check if SCRIPT-PATH is already in HOOK-ENTRIES."
  (seq-some
   (lambda (entry)
     (let ((inner-hooks (alist-get 'hooks entry)))
       (seq-some
        (lambda (h)
          (equal (alist-get 'command h) script-path))
        inner-hooks)))
   hook-entries))

;;; Hook callbacks (called by emacsclient from the hook script)

(defun emacs-superset-hooks--set-status (workspace-path status)
  "Set agent status to STATUS for workspace at WORKSPACE-PATH.
Also updates the timestamp and refreshes the dashboard if visible."
  (when-let ((ws (emacs-superset--get-workspace workspace-path)))
    (setf (emacs-superset-workspace-agent-status ws) status)
    (setf (emacs-superset-workspace-status-changed-at ws) (float-time))
    ;; Schedule lightweight dashboard redraw on next command loop iteration
    (when (fboundp 'emacs-superset-dashboard-redraw)
      (run-at-time 0 nil #'emacs-superset-dashboard-redraw))))

(defun emacs-superset-hooks--on-activity (workspace-path)
  "Handle UserPromptSubmit/PreToolUse/PostToolUse for WORKSPACE-PATH."
  (emacs-superset-hooks--set-status workspace-path 'running))

(defun emacs-superset-hooks--on-stop (workspace-path)
  "Handle Stop hook event for WORKSPACE-PATH."
  (emacs-superset-hooks--set-status workspace-path 'done))

(defun emacs-superset-hooks--on-error (workspace-path)
  "Handle StopFailure hook event for WORKSPACE-PATH."
  (emacs-superset-hooks--set-status workspace-path 'error))

(defun emacs-superset-hooks--on-notification (workspace-path)
  "Handle Notification/PermissionRequest hook event for WORKSPACE-PATH."
  (emacs-superset-hooks--set-status workspace-path 'waiting))

(defun emacs-superset-hooks--on-session-end (workspace-path)
  "Handle SessionEnd hook event for WORKSPACE-PATH."
  (emacs-superset-hooks--set-status workspace-path 'idle))

;;; Cleanup

(defun emacs-superset-hooks-uninstall (workspace)
  "Remove the emacs-superset hook script from WORKSPACE."
  (let* ((path (emacs-superset-workspace-path workspace))
         (script-path (expand-file-name
                       (concat ".claude/" emacs-superset-hooks--script-name)
                       path)))
    (when (file-exists-p script-path)
      (delete-file script-path))))

(provide 'emacs-superset-hooks)
;;; emacs-superset-hooks.el ends here
