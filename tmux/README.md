# Tmux

tmux is fun!

## My Key mappings

Use `C-A` as the PREFIX in tmux.

```
Root mode:

M-b switch to last session
M-p switch to next window
M-q switch to next pane
M-d detach current client
M-f switch to another session via fzf
M-m mark current pane
M-m unmark current pane
M-< join marked pane to current window
M-w switch to work session

Prefix key table:

PRFIX { swap current window with previous window
PRFIX } swap current window with next window
PREFIX S source update_session_path
PREFIX O list all hooks
PREFIX w display popup of work session
PREFIX ? list all key-bindings
PREFIX x kill current pane
PREFIX X kill current window
PREFIX r reload tmux configuration
PREFIX > swap current pane with next one
PREFIX < swap current pane with previous one
PREFIX Enter enter copy mode
PREFIX m monitor-activity
PREFIX y synchronize-panes
PREFIX M-p popup of ipython
PREFIX C-w move current window to another session
PREFIX C-p move and append current pane to another session as a window
```
