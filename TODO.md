# TODO

## Improve Workspace Switching UX

Goal: make switching between emacs-superset workspaces fast from Doom/Evil,
especially when starting in an `eat` terminal buffer.

1. [x] Make dashboard `RET` focus the workspace terminal.
   - When point is on a workspace row, switch to that workspace tab and select
     the agent/main terminal window.
   - Avoid landing focus back in the dashboard side window.

2. [x] Preserve dashboard point by workspace identity.
   - Remember the workspace at point before dashboard refresh.
   - After redraw, jump back to that workspace section instead of restoring only
     the raw buffer position.

3. [ ] Add a direct workspace switch command.
   - Provide a command that uses `completing-read` over workspace names.
   - Switching should land in the workspace terminal by default.

4. [ ] Add an ace-window-style quick picker.
   - Show short one-key labels for available workspaces.
   - Read one key and switch directly to the selected workspace terminal.
   - Consider using `avy` when available, or a simple temporary dashboard
     annotation fallback.

5. [ ] Add terminal-friendly bindings.
   - Provide/recommend a binding usable from terminal insert mode, without
     requiring `ESC`.
   - Include Doom/Evil examples in the README once the commands exist.
