;;; emacs-superset-hooks.el --- Agent hook integrations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thomas

;; This file is part of emacs-superset.

;;; Commentary:

;; Integrates with agent hook systems to detect agent state changes in real
;; time.  For Claude Code, we install a hook script and register it for:
;;
;;   - UserPromptSubmit: user sends a message     -> running
;;   - PreToolUse:       Claude invokes a tool     -> running
;;   - Stop:             Claude finishes a turn    -> done
;;   - StopFailure:      API error during turn     -> error
;;   - Notification:     needs permission/input    -> waiting
;;   - SessionEnd:       session terminates        -> idle
;;
;; For Codex, we install a global hook script in CODEX_HOME and register it for:
;;
;;   - SessionStart:     session starts            -> idle
;;   - UserPromptSubmit: user sends a message      -> running
;;   - PreToolUse:       Codex invokes a tool      -> running
;;   - PostToolUse:      Codex completed a tool    -> running
;;   - PermissionRequest:needs permission/input    -> waiting
;;   - Stop:             Codex finishes a turn     -> done
;;
;; Hook scripts call emacsclient --eval to update workspace status.
;; Requires (server-start) in Emacs.

;;; Code:

(require 'emacs-superset-core)
(require 'json)

;;; Hook script

(defconst emacs-superset-hooks--script-name "emacs-superset-hook.sh"
  "Name of the hook script installed in each workspace.")

(defconst emacs-superset-hooks--codex-script-name "emacs-superset-codex-hook.sh"
  "Name of the hook script installed in CODEX_HOME.")

(defconst emacs-superset-hooks--events
  '(UserPromptSubmit PreToolUse PostToolUse PermissionRequest
    Stop StopFailure Notification SessionEnd)
  "Claude Code hook events we register for.")

(defconst emacs-superset-hooks--codex-events
  '(SessionStart UserPromptSubmit PreToolUse PermissionRequest PostToolUse Stop)
  "Codex hook events we register for.")

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

(defun emacs-superset-hooks--codex-script-content ()
  "Return the Codex hook script content.
The script reads JSON from stdin, extracts hook_event_name and cwd, and calls
emacsclient to update the matching workspace status."
  "#!/bin/sh
# emacs-superset Codex hook - auto-generated, do not edit
# Updates agent status in emacs-superset dashboard via emacsclient

EVENT=$(cat)
HOOK_EVENT=$(printf '%s' \"$EVENT\" | sed -n 's/.*\"hook_event_name\" *: *\"\\([^\"]*\\)\".*/\\1/p')
CWD=$(printf '%s' \"$EVENT\" | sed -n 's/.*\"cwd\" *: *\"\\([^\"]*\\)\".*/\\1/p')

if [ -z \"$HOOK_EVENT\" ] || [ -z \"$CWD\" ]; then
  exit 0
fi

ec() { emacsclient --no-wait --eval \"$1\" >/dev/null 2>&1; }

case \"$HOOK_EVENT\" in
  SessionStart)
    ec \"(emacs-superset-hooks--codex-on-session-start \\\"$CWD\\\")\"
    ;;
  UserPromptSubmit|PreToolUse|PostToolUse)
    ec \"(emacs-superset-hooks--codex-on-activity \\\"$CWD\\\")\"
    ;;
  PermissionRequest)
    ec \"(emacs-superset-hooks--codex-on-notification \\\"$CWD\\\")\"
    ;;
  Stop)
    ec \"(emacs-superset-hooks--codex-on-stop \\\"$CWD\\\")\"
    ;;
esac

exit 0
")

;;; Hook installation

(defun emacs-superset-hooks-install (workspace)
  "Install supported agent hooks for WORKSPACE."
  (emacs-superset-hooks-install-claude workspace)
  (emacs-superset-hooks-install-codex))

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

(defun emacs-superset-hooks-install-claude (workspace)
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

(defun emacs-superset-hooks--codex-home ()
  "Return Codex home directory."
  (expand-file-name (or (getenv "CODEX_HOME") "~/.codex")))

(defun emacs-superset-hooks--codex-config-file ()
  "Return Codex config.toml path."
  (expand-file-name "config.toml" (emacs-superset-hooks--codex-home)))

(defun emacs-superset-hooks--codex-hooks-file ()
  "Return Codex hooks.json path."
  (expand-file-name "hooks.json" (emacs-superset-hooks--codex-home)))

(defun emacs-superset-hooks--codex-script-path ()
  "Return Codex hook script path."
  (expand-file-name emacs-superset-hooks--codex-script-name
                    (emacs-superset-hooks--codex-home)))

(defun emacs-superset-hooks-install-codex ()
  "Install global Codex hooks for emacs-superset.
Writes CODEX_HOME/hooks.json and enables the `codex_hooks' feature in
CODEX_HOME/config.toml."
  (condition-case err
      (let* ((codex-home (emacs-superset-hooks--codex-home))
             (script-path (emacs-superset-hooks--codex-script-path)))
        (make-directory codex-home t)
        (with-temp-file script-path
          (insert (emacs-superset-hooks--codex-script-content)))
        (set-file-modes script-path #o755)
        (emacs-superset-hooks--codex-enable-feature
         (emacs-superset-hooks--codex-config-file))
        (emacs-superset-hooks--codex-write-hooks
         (emacs-superset-hooks--codex-hooks-file)
         script-path)
        (message "emacs-superset: Installed Codex hooks")
        t)
    (error
     (message "emacs-superset: Failed to install Codex hooks: %s" err)
     nil)))

(defun emacs-superset-hooks--codex-enable-feature (config-path)
  "Ensure CONFIG-PATH enables [features].codex_hooks."
  (let ((text (if (file-exists-p config-path)
                  (with-temp-buffer
                    (insert-file-contents config-path)
                    (buffer-string))
                "")))
    (setq text
          (cond
           ((string-match-p "^codex_hooks[ \t]*=[ \t]*true[ \t]*$" text)
            text)
           ((string-match "^codex_hooks[ \t]*=[ \t]*false[ \t]*$" text)
            (replace-match "codex_hooks = true" nil nil text))
           ((string-match "^\\[features\\][ \t]*$" text)
            (replace-match "[features]\ncodex_hooks = true" nil nil text))
           (t
            (concat (string-remove-suffix "\n" text)
                    (unless (string-empty-p text) "\n\n")
                    "[features]\ncodex_hooks = true\n"))))
    (with-temp-file config-path
      (insert text))))

(defun emacs-superset-hooks--codex-write-hooks (hooks-path script-path)
  "Merge emacs-superset Codex hook entries into HOOKS-PATH using SCRIPT-PATH."
  (let* ((json-object-type 'alist)
         (json-key-type 'symbol)
         (json-array-type 'vector)
         (settings (if (file-exists-p hooks-path)
                       (condition-case nil
                           (json-read-file hooks-path)
                         (error nil))
                     nil)))
    (setq settings (emacs-superset-hooks--codex-merge-hooks settings script-path))
    (with-temp-file hooks-path
      (insert (json-encode settings)))))

(defun emacs-superset-hooks--codex-merge-hooks (settings script-path)
  "Merge Codex hook entries into SETTINGS using SCRIPT-PATH."
  (let* ((settings (or settings '()))
         (hooks (alist-get 'hooks settings))
         (hook `((type . "command")
                 (command . ,script-path)
                 (timeout . 5)
                 (statusMessage . "Updating emacs-superset"))))
    (dolist (event emacs-superset-hooks--codex-events)
      (let* ((event-hooks (alist-get event hooks))
             (entry (or (emacs-superset-hooks--codex-find-entry event-hooks)
                        '((matcher . "")
                          (hooks . []))))
             (handlers (alist-get 'hooks entry)))
        (unless (emacs-superset-hooks--codex-handler-installed-p handlers script-path)
          (setf (alist-get 'hooks entry)
                (vconcat (or handlers []) (vector hook))))
        (unless (seq-some (lambda (existing) (eq existing entry)) event-hooks)
          (setq event-hooks (vconcat (or event-hooks []) (vector entry))))
        (setf (alist-get event hooks) event-hooks)))
    (setf (alist-get 'hooks settings) hooks)
    settings))

(defun emacs-superset-hooks--codex-find-entry (event-hooks)
  "Find the catch-all entry in Codex EVENT-HOOKS."
  (seq-find (lambda (entry)
              (let ((matcher (alist-get 'matcher entry)))
                (or (null matcher) (equal matcher "") (equal matcher "*"))))
            event-hooks))

(defun emacs-superset-hooks--codex-handler-installed-p (handlers script-path)
  "Return non-nil if HANDLERS already includes SCRIPT-PATH."
  (seq-some (lambda (handler)
              (equal (alist-get 'command handler) script-path))
            handlers))

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
    (emacs-superset--set-agent-status ws status)))

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

;;; Codex hook callbacks (called by emacsclient from the hook script)

(defun emacs-superset-hooks--codex-workspace-for-path (path)
  "Return the registered workspace whose path contains PATH."
  (let ((path (emacs-superset--normalize-path path)))
    (or (emacs-superset--get-workspace path)
        (seq-find
         (lambda (ws)
           (file-in-directory-p
            path
            (file-name-as-directory
             (emacs-superset--normalize-path
              (emacs-superset-workspace-path ws)))))
         (emacs-superset--all-workspaces)))))

(defun emacs-superset-hooks--codex-set-status (path status)
  "Set Codex workspace status to STATUS for session CWD PATH."
  (when-let ((ws (emacs-superset-hooks--codex-workspace-for-path path)))
    (setf (emacs-superset-workspace-agent-type ws) 'codex)
    (emacs-superset--set-agent-status ws status)))

(defun emacs-superset-hooks--codex-on-session-start (path)
  "Handle Codex SessionStart hook for PATH."
  (emacs-superset-hooks--codex-set-status path 'idle))

(defun emacs-superset-hooks--codex-on-activity (path)
  "Handle Codex activity hooks for PATH."
  (emacs-superset-hooks--codex-set-status path 'running))

(defun emacs-superset-hooks--codex-on-notification (path)
  "Handle Codex PermissionRequest hook for PATH."
  (emacs-superset-hooks--codex-set-status path 'waiting))

(defun emacs-superset-hooks--codex-on-stop (path)
  "Handle Codex Stop hook for PATH."
  (emacs-superset-hooks--codex-set-status path 'done))

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
