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

|   Mode        | Keybinding |              Action                |
|:-------------:|:----------:|:----------------------------------:|
| Default       | C-a        | Set prefix key to `C-a`             |
| Copy-mode     | MouseDown1Pane | Select pane                     |
| Copy-mode     | MouseDrag1Pane | Start selection, then send keys |
| Copy-mode     | MouseDragEnd1Pane | Copy and cancel selection     |
| Copy-mode     | WheelUpPane | Scroll up                         |
| Copy-mode     | WheelDownPane | Scroll down                     |
| Copy-mode     | DoubleClick1Pane | Select word, copy and cancel |
| Copy-mode     | TripleClick1Pane | Select line, copy and cancel |
| Root          | MouseDown1Pane | Select pane and send keys       |
| Root          | MouseDown3Pane | Display menu                    |
| Root          | MouseDown3Status | Display menu                   |
| Root          | MouseDown2Pane | Select pane, paste or send keys |
| Root          | MouseDown1Status | Select window                  |
| Prefix        | %          | Horizontal split window          |
| Prefix        | "          | Vertical split window            |
| Prefix        | c          | Create new window                |
| Prefix        | ?          | List all keybindings             |
| Prefix        | r          | Reload tmux configuration        |
| Prefix        | x          | Kill current pane                |
| Prefix        | X          | Kill current window              |
| Prefix        | s          | Split pane                        |
| Prefix        | w          | Choose tree (workspace)          |
| Prefix        | C-w        | Move current window to another session |
| Prefix        | M-p        | Open IPython in popup            |
| Prefix        | M-f        | Switch client to another session |
| Prefix        | M-b        | Switch to last session           |
| Prefix        | M-d        | Detach client                    |
| Prefix        | F1         | Select window 10                 |
| Prefix        | F2         | Select window 11                 |
| Prefix        | F3         | Select window 12                 |
| Prefix        | F4         | Select window 13                 |
| Prefix        | F5         | Select window 14                 |
| Prefix        | F6         | Select window 15                 |
| Prefix        | F7         | Select window 16                 |
| Prefix        | F8         | Select window 17                 |
| Prefix        | F9         | Select window 18                 |
| Prefix        | F10        | Select window 19                 |
| Prefix        | F11        | Select window 20                 |
| Prefix        | F12        | Select window 21                 |
| Prefix        | h          | Resize pane left                 |
| Prefix        | j          | Resize pane down                 |
| Prefix        | k          | Resize pane up                   |
| Prefix        | l          | Resize pane right                |
| Prefix        | <          | Swap current pane with the previous one |
| Prefix        | S-Up       | Select pane up                   |
| Prefix        | S-Down     | Select pane down                 |
| Prefix        | S-Left     | Select pane left                 |
| Prefix        | S-Right    | Select pane right                |
| Copy-mode-vi  | v          | Start selection                  |
| Copy-mode-vi  | C-v        | Toggle rectangle selection       |
| Copy-mode-vi  | y          | Copy selection to clipboard (Linux) |
| Copy-mode-vi  | Y          | Copy selection to clipboard via OSC 52 |
| Default       | C-p        | Break pane into a window         |
| Default       | C-w        | Move current window to another session |
| Default       | M-w        | New session workspace            |
| Default       | M-t        | Choose tree                      |
| Default       | r          | Reload tmux configuration        |
| Default       | C-d        | Detach from session              |
| Default       | M-Space    | Activate selection               |
| Default       | M-h        | Resize pane left                 |
| Default       | M-j        | Resize pane down                 |
| Default       | M-k        | Resize pane up                   |
| Default       | M-l        | Resize pane right                |
| Default       | C-r        | Reload tmux configuration        |
| Default       | M-m        | Mark current pane                |
| Default       | M-M        | Mark all panes                   |
| Default       | m          | Set synchronize-panes flag       |

