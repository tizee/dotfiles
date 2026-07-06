# Zsh Startup Performance

Benchmark-driven optimization notes for this config. Last measured 2026-07-06 on
an Apple Silicon Mac.

## Current Numbers

| Scenario | Command | Mean (hyperfine, 15 runs) |
|----------|---------|---------------------------|
| Interactive (tmux new pane) | `zsh -i -c exit` | ~40ms |
| Login (new terminal window) | `zsh -l -i -c exit` | ~48ms |

History:

| Date | Interactive | Login | Change |
|------|-------------|-------|--------|
| ~2021 (pre-optimization) | ~180ms | - | oh-my-zsh era baseline |
| ~2023 | ~36ms | - | custom config, zwc compilation, lazy loading |
| 2026-07-06 (before fix) | 53ms | 92ms | compinit cache had silently broken |
| 2026-07-06 (after fix) | 40ms | 48ms | compinit touch fix, brew shellenv inlined, forks removed |

## Methodology: Benchmark-Driven Optimization

Never optimize blind. The loop is:

1. **Measure the whole**: wall-clock startup time with statistical rigor.

   ```sh
   hyperfine --warmup 3 --runs 15 'zsh -i -c exit' 'zsh -l -i -c exit'
   # or without hyperfine:
   ./benchmark-shell-startup.zsh 10
   ```

   Benchmark *both* shell flavors. `zsh -i` covers tmux panes (zshenv + zshrc);
   `zsh -l -i` covers new terminal windows (zshenv + zprofile + zshrc + zlogin).
   A hotspot in zprofile is invisible to an interactive-only benchmark.

2. **Profile the parts**: find where the time goes.

   ```zsh
   # Function-level profiling (finds compinit, prompt init, plugin costs):
   zsh -i -c 'zmodload zsh/zprof; source ~/.zshrc; zprof' | head -20

   # Line-level tracing with timestamps (finds slow top-level statements):
   zsh -i -c 'PS4=$'\''+%D{%s.%6.}:%N:%i> '\''; setopt xtrace; source ~/.zshrc' 2>/tmp/trace.log
   ```

3. **Measure suspects in isolation**: before touching anything, confirm the
   cost of a candidate with `$EPOCHREALTIME`:

   ```zsh
   zsh -c 'zmodload zsh/datetime; t0=$EPOCHREALTIME
           eval "$(/opt/homebrew/bin/brew shellenv)"
           printf "%.1fms\n" $(( ($EPOCHREALTIME-t0)*1000 ))'
   # => 19.9ms
   ```

4. **Change one thing, re-benchmark, keep or revert.** A change that saves
   less than the run-to-run noise (~3-5ms here) is not worth its complexity.

## Techniques Used in This Config

### 1. compinit dump caching — with the mtime pitfall

`compinit` scans every directory in `$fpath` and runs `compaudit` security
checks (~20ms). `compinit -C` skips all of that and just reads the cached
`~/.zcompdump` (~7ms). This config takes the fast path when the dump is less
than 24 hours old:

```zsh
if [[ -f "$HOME/.zcompdump"(#qNmh-24) ]]; then
  compinit -C
else
  compinit
  touch "$HOME/.zcompdump"   # <-- essential, see below
fi
```

**The pitfall**: `compinit` only rewrites the dump file when its *content*
changes (new/removed completions). If fpath is stable, the file's mtime never
advances, the 24h check fails forever, and every startup silently pays the
full compinit + compaudit cost. This exact bug cost this config ~20ms per
startup for two weeks before profiling caught it. The `touch` keeps the cache
signal valid: only the first shell of the day does the full rebuild.

Lesson: when a cache validity check depends on mtime, *you* own the mtime.
Don't assume the producer updates it.

### 2. Replace `eval "$(tool shellenv/init)"` with static exports

`eval "$(/opt/homebrew/bin/brew shellenv)"` forks brew (a ruby-adjacent shim)
on every login shell: **~20ms**. Its output is entirely static for a fixed
install location, so it is inlined in zprofile:

```zsh
export HOMEBREW_PREFIX="/opt/homebrew"
export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY="/opt/homebrew"
fpath[1,0]="/opt/homebrew/share/zsh/site-functions"
export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}"
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
```

This is safe here because `setopt noglobalrcs` (zshenv) already skips
`/etc/zprofile`'s path_helper, so PATH is fully hand-assembled anyway.

The same pattern applies to any `eval "$(X init zsh)"` (rbenv, pyenv, conda,
mamba, zoxide...): either inline the static parts, cache the output to a file
and source it, or lazy-load behind a wrapper function. The commented-out
conda/mamba/rbenv blocks in zshrc are casualties of this rule.

### 3. Prefer zsh builtins over forks

Every `$(command)` costs a fork+exec, roughly 1-3ms each on macOS. Zsh has
builtin equivalents for most startup-time queries:

| Fork | Builtin replacement | Where used |
|------|--------------------|------------|
| `$(uname -s)` | `case $OSTYPE in darwin*)...` | zshenv |
| `$(tty)` | `$TTY` | zprofile (`GPG_TTY`) |
| `$(date)` | `print -P '%D{...}'` (prompt expansion) | zprofile banner |
| `uname -r \| grep microsoft` | guard with `$is_Linux` first | zprofile WSL check |
| `$(dirname $0)` | `${0:h}` / `${${(%):-%N}:A:h}` | zshrc prompt path |
| `command -v x` (external) | `(( ${+commands[x]} ))` | zshrc tmux check |

Individually small; collectively ~8ms on the login path.

### 4. Defer non-critical plugin loading

zsh-syntax-highlighting only matters once you start typing, not before the
first prompt renders. It is loaded from a one-shot `precmd` hook:

```zsh
function _load_syntax_highlighting() {
  source "$ZSHDIR/vendor/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
  add-zsh-hook -d precmd _load_syntax_highlighting  # self-remove
}
add-zsh-hook precmd _load_syntax_highlighting
```

This moves ~4ms off the prompt-visible critical path. The same trick works for
any plugin that only affects interactive editing.

### 5. Autoload functions instead of sourcing them

Every function in `autoloaded/` is registered with `autoload -Uz` (name only,
~0 cost) and its body is only parsed on first call:

```zsh
fpath=($ZSHDIR/autoloaded $HOME/.config/zfunc "${fpath[@]}")
autoload -Uz $fpath[1]/*(.:t)
```

Sourcing 20+ function files eagerly would parse thousands of lines at startup;
autoloading defers that to first use.

### 6. Compile scripts to wordcode (`.zwc`), off the critical path

zlogin runs `zwc_watcher &!` — a **disowned background job** that
`zrecompile`s zshrc/zprofile/zshenv, all plugins, autoloaded functions, and
the completion dump. Zsh transparently prefers a `.zwc` file when it is newer
than its source, skipping the parse step on subsequent startups.

Two details matter:

- It runs in zlogin (last startup file) and in the background, so compilation
  cost is never paid on the prompt-visible path.
- `zrecompile -p` only recompiles when the source is newer — idempotent.

### 7. Keep the prompt synchronous-cheap

The prompt uses gitstatus (a daemon holding the repo index) instead of forking
`git status` per prompt. Startup cost is a one-time `gitstatus_start`
(~0.8ms); per-prompt cost is an IPC roundtrip instead of a fork.

## Benchmark Tooling in This Directory

| Script | Purpose |
|--------|---------|
| `benchmark-shell-startup.zsh [runs]` | Startup wall-clock: avg/median/min/max, no dependencies |
| `benchmark-plugins.zsh` | Per-plugin load cost |
| `bench` / `bench-with-expect` / `expect-run` | zimfw-style benchmark (spawns a real pty via expect) |

Prefer `hyperfine` when available — it handles warmup, outlier detection, and
statistical comparison between two variants:

```sh
hyperfine --warmup 3 'zsh -i -c exit' 'zsh -l -i -c exit'
```

## Pitfalls Checklist

- [ ] Benchmarking only `zsh -i` and missing zprofile/zlogin hotspots
- [ ] mtime-based cache whose producer never bumps the mtime (compinit)
- [ ] `eval "$(tool init)"` accumulating over the years, one per tool
- [ ] Trusting a years-old "already optimized" label — caches rot silently;
      re-profile occasionally
- [ ] Comparing single runs instead of distributions (first run after edit
      pays .zwc recompile + cold cache; always warm up)
- [ ] `zsh -i -c exit` has no TTY: job control (`setopt monitor`) is off, so
      gitstatus prints an init error during benchmarks — harmless artifact,
      not a regression
