---
name: tmux
description: "Remote control tmux sessions for interactive CLIs by sending keystrokes and scraping output."
license: WTFPL
---

# tmux Skill

Programmable terminal multiplexer for interactive work. Use tmux to remote control interactive CLIs (python, gdb, etc.) by sending keystrokes and scraping pane output.

## Quickstart

**Always use `create-session.sh` to create sessions** - it handles socket management, prompt detection, and error handling automatically.

```bash
# Python REPL session
tools/create-session.sh -n python -c clean \
  -p 'PYTHON_BASIC_REPL=1 python3 -q' -w '^>>> '

# After creation, ALWAYS provide monitoring commands:
To monitor this session yourself:
  tmux -S "/tmp/claude-tmux-sockets/python.sock" attach -t python

Or to capture the output once:
  tmux -S "/tmp/claude-tmux-sockets/python.sock" capture-pane -p -J -t python:0.0 -S -200
```

This must ALWAYS be printed right after a session was started and once again at the end of the tool loop. But the earlier you send it, the happier the user will be.

### Manual Session Creation (Advanced)

If you need full control, you can create sessions manually:

```bash
SOCKET_DIR=${TMPDIR:-/tmp}/claude-tmux-sockets
mkdir -p "$SOCKET_DIR"
SOCKET="$SOCKET_DIR/claude.sock"
SESSION=claude-python

tmux -S "$SOCKET" new -d -s "$SESSION" -n shell
tmux -S "$SOCKET" send-keys -t "$SESSION":0.0 -- 'PYTHON_BASIC_REPL=1 python3 -q' Enter
tmux -S "$SOCKET" capture-pane -p -J -t "$SESSION":0.0 -S -200
tmux -S "$SOCKET" kill-session -t "$SESSION"
```

## Socket convention

- Place tmux sockets under `CLAUDE_TMUX_SOCKET_DIR` (defaults to `${TMPDIR:-/tmp}/claude-tmux-sockets`) and use `tmux -S "$SOCKET"` so we can enumerate/clean them. Create the dir first: `mkdir -p "$CLAUDE_TMUX_SOCKET_DIR"`.
- Default socket path to use unless you must isolate further: `SOCKET="$CLAUDE_TMUX_SOCKET_DIR/claude.sock"`.
- **Never use default `/tmp/tmux-*` sockets.** Use per-session sockets in this directory.

## Targeting panes and naming

- Target format: `{session}:{window}.{pane}`, defaults to `:0.0` if omitted. Keep names short (e.g., `claude-py`, `claude-gdb`).
- Use `-S "$SOCKET"` consistently to stay on the private socket path. If you need user config, drop `-f /dev/null`; otherwise `-f /dev/null` gives a clean config.
- Inspect: `tmux -S "$SOCKET" list-sessions`, `tmux -S "$SOCKET" list-panes -a`.

## Finding sessions

**Always use the helper** to find created sockets:
- List sessions on your active socket: `tools/find-sessions.sh -S "$SOCKET"`
- Find all sockets: `tools/find-sessions.sh --all` (scans `CLAUDE_TMUX_SOCKET_DIR`)
- Filter by name: `tools/find-sessions.sh -S "$SOCKET" -q "pattern"`

## Sending input safely

- Prefer literal sends to avoid shell splitting: `tmux -S "$SOCKET" send-keys -t target -l -- "$cmd"`
- **Note**: When sending multiple lines with `-l` flag, include `\n` for newlines, then send Enter separately to execute
- When composing inline commands, use single quotes or ANSI C quoting to avoid expansion: `tmux -S "$SOCKET" send-keys -t target -- $'python3 -m http.server 8000'`.
- To send control keys: `tmux -S "$SOCKET" send-keys -t target C-c`, `C-d`, `C-z`, `Escape`, etc.
- Send Enter: `tmux -S "$SOCKET" send-keys -t target -- "cmd" Enter`
- **IMPORTANT**: The `-l` (literal) flag treats ALL input as literal text. You cannot combine `-l` with special keys like `Enter`, `C-c`, etc. in the same command.

## Watching output

- Capture recent history (joined lines to avoid wrapping artifacts): `tmux -S "$SOCKET" capture-pane -p -J -t target -S -200`.
- For continuous monitoring, poll with the helper script (below) instead of `tmux wait-for` (which does not watch pane output).
- You can also temporarily attach to observe: `tmux -S "$SOCKET" attach -t "$SESSION"`; detach with `Ctrl+b d`.
- When giving instructions to a user, **explicitly print a copy/paste monitor command** alongside the action don't assume they remembered the command.

## Spawning Processes

Some special rules for processes:

- When asked to debug, use lldb by default
- When starting a python interactive shell, always set the `PYTHON_BASIC_REPL=1` environment variable. This is very important as the non-basic console interferes with your send-keys.

## Synchronizing / waiting for prompts

- Use timed polling to avoid races with interactive tools. Example: wait for a Python prompt before sending code:
  ```bash
  tools/wait-for-text.sh -t "$SESSION":0.0 -p '^>>>' -T 15 -l 4000
  ```
- For long-running commands, poll for completion text (`"Type quit to exit"`, `"Program exited"`, etc.) before proceeding.

## Interactive tool recipes

- **Python REPL**: `tmux -S "$SOCKET" send-keys -- 'python3 -q' Enter`; wait for `^>>>`; send code with `-l`; interrupt with `C-c`. Always with `PYTHON_BASIC_REPL=1`.
- **GDB**: `tmux -S "$SOCKET" send-keys -- 'gdb --quiet ./a.out' Enter`; disable paging `tmux -S "$SOCKET" send-keys -- 'set pagination off' Enter`; break with `C-c`; issue `bt`, `info locals`, etc.; exit via `quit` then confirm `y`.
- **Other TTY apps** (ipdb, psql, mysql, node, bash): same pattern—start the program, poll for its prompt, then send literal text and Enter.

## Primary Tool: create-session.sh

**Always use `create-session.sh` instead of manual tmux commands** - it handles all the details automatically.

```bash
tools/create-session.sh -n NAME -c clean|user|custom [options]

Options:
  -n NAME        Session name
  -c LEVEL       clean (no config), user (your keys), custom
  -p PROGRAM     Program to launch
  -w PATTERN     Wait for regex pattern
  -t TIMEOUT     Timeout (default 15)
  -s SOCKET_DIR  Socket directory
  -f CONFIG      Custom config file
  -h             Show help
```

**Recommended usage examples:**

```bash
# Python (always use PYTHON_BASIC_REPL=1)
tools/create-session.sh -n python -c clean \
  -p 'PYTHON_BASIC_REPL=1 python3 -q' -w '^>>> '

# GDB
tools/create-session.sh -n gdb -c user \
  -p 'gdb --quiet ./program' -w '^(gdb) '

# PostgreSQL
tools/create-session.sh -n db -c clean \
  -p 'psql -h localhost mydb' -w 'mydb=#'
```

## Helper: wait-for-text.sh

`tools/wait-for-text.sh` polls a pane for a regex (or fixed string) with a timeout. Works on Linux/macOS with bash + tmux + grep.

```bash
tools/wait-for-text.sh -t TARGET -p PATTERN [-s SOCKET] [options]
```

**Required:**
- `-t`, `--target` - tmux target (session:window.pane), required
- `-p`, `--pattern` - regex pattern to look for, required

**Optional:**
- `-s`, `--socket` - tmux socket path
- `-F`, `--fixed` - treat pattern as a fixed string (grep -F)
- `-T`, `--timeout` - seconds to wait (integer, default: 15)
- `-i`, `--interval` - poll interval in seconds (default: 0.5)
- `-l`, `--lines` - number of history lines to inspect (integer, default: 1000)
- `-h`, `--help` - show help

**Exit codes:**
- Exits 0 on first match, 1 on timeout. On failure prints the last captured text to stderr to aid debugging.

## Helper Scripts

### Find Sessions

```bash
tools/find-sessions.sh --all                  # All agent sockets
tools/find-sessions.sh -S SOCKET -q "pattern" # Filter by name
```

## Cleanup

- Kill a session when done: `tmux -S "$SOCKET" kill-session -t "$SESSION"`.
- Kill all sessions on a socket: `tmux -S "$SOCKET" list-sessions -F '#{session_name}' | xargs -r -n1 tmux -S "$SOCKET" kill-session -t`.
- Remove everything on the private socket: `tmux -S "$SOCKET" kill-server`.

## Troubleshooting

- **Timeout:** verify pattern, increase `-T`
- **Python:** check `PYTHON_BASIC_REPL=1`, verify `^>>> `
- **Cannot attach:** `tools/find-sessions.sh -S "$SOCKET"`
- **Session exists:** kill first
