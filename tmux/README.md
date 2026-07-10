# Tmux

tmux is fun!

## My Key mappings

Use `C-A` as the PREFIX in tmux.

## Quick Start

Prefix = `C-a`. Tables below are grouped by daily operation; entries marked "custom" are keybindings changed by this repo (the rest are tmux defaults).
See all keybindings: `prefix ?` or `tmux list-keys`.

### Window (tabs at the top)

| Key | Action | Notes |
|-----|--------|-------|
| `prefix c` | New window (inserted after current) | custom `new-window -a` |
| `prefix C` | New window with name prompt | custom |
| `prefix 0`~`9` | Jump to window N | |
| `prefix F1`~`F12` | Jump to windows 10~21 | custom |
| `prefix n` / `prefix p` | Next / previous window | |
| `prefix ,` | Rename current window | default |
| `prefix w` | Window tree selector | default choose-tree |
| `prefix &` / `prefix X` | Kill window (with / without confirm) | `X` custom |
| `prefix {` / `prefix }` | **Swap window** with left / right neighbor | custom swap-window |
| `prefix .` | Move current window to a target index | default, prompts for target |
| no-prefix `M-p` | Next window | custom |

### Pane (splits)

| Key | Action | Notes |
|-----|--------|-------|
| `prefix %` | Split horizontally (side by side) | custom, preserves cwd |
| `prefix "` | Split vertically (stacked) | custom, preserves cwd |
| `prefix arrow` | Move focus between panes | default |
| no-prefix `S-arrow` | Move focus between panes (no prefix) | custom |
| no-prefix `M-q` | Cycle to next pane | custom |
| `prefix ;` | Jump to last active pane | default |
| `prefix z` | Zoom / unzoom current pane | default zoom |
| `prefix space` | Cycle layout | default next-layout |
| `prefix q` | Show pane numbers | default |
| `prefix x` | Kill current pane | custom, no confirmation |
| `prefix H`/`J`/`K`/`L` | Resize pane left/down/up/right | custom, **uppercase** |
| `prefix <` | Swap with previous pane | custom swap-pane |

### Session

| Key | Action | Notes |
|-----|--------|-------|
| `prefix s` | Session tree selector | default choose-tree |
| `prefix d` / no-prefix `M-d` | Detach current client | |
| `prefix $` | Rename session | default |
| `prefix (` / `prefix )` | Previous / next session | default |
| no-prefix `M-b` | Switch to last session | custom |
| no-prefix `M-f` | fzf pick and switch session | custom popup |

### Move & Rearrange

| Goal | Key | Details |
|------|-----|---------|
| **Send current pane to a target window** | `prefix g` | Prompts `send pane to:`, enter target window index/name (`join-pane -t`, custom) |
| Break current pane into its own window | `prefix !` | break-pane, custom |
| Link a window from another session | `prefix e` | Enter `session:window` (link-window, custom) |
| Move current window to a target index | `prefix .` | default move-window, prompts for target |
| Swap current window with left/right neighbor | `prefix {` / `}` | custom |
| Move current window to **another session** | `prefix C-w` | fzf pick target session (custom popup) |
| Break current pane into a window in **another session** | `prefix C-p` | fzf pick target session (custom popup) |
| Join marked pane into current window | no-prefix `M-<` | Mark source pane with `M-m` first, then execute (custom) |

### Copy / Capture

| Key | Action | Notes |
|-----|--------|-------|
| `prefix Enter` or `prefix [` | Enter copy mode | `Enter` custom |
| copy-mode `v` / `C-v` | Start selection / rectangle selection | custom vi-style |
| copy-mode `y` / `Y` | Copy to clipboard / via OSC 52 | |
| `prefix ]` | Paste buffer | custom `paste -p` |
| `prefix Y` | Capture mode (fork built-in, pick URL/path/etc.) | |

### Misc

| Key | Action |
|-----|--------|
| `prefix r` | Reload config |
| `prefix ?` | List all keybindings |
| `prefix :` | tmux command prompt |
| `prefix o` | Show current options (note: default "next pane" is overridden to show-options) |

---

## Complete Keybinding Reference

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

