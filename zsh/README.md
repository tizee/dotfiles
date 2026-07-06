# tz-zshconfig

Hand-rolled zsh config. Startup: ~40ms interactive, ~48ms login (see
[PERFORMANCE.md](./PERFORMANCE.md) for methodology and techniques).

## Why not use `oh-my-zsh`?

It prevent me from customizing the Zsh the way I want, i.e., setup my own zsh configuration from bottom up.

Another drawback for me is that each time after updating oh-my-zsh I could find lots of useless files has been modified.

For pursuing a minimal and clean config, I decide to setup my own config. It's not easy but really fun.

By setting up your own config, you will learn:

1. Zsh prompt configuration

2. Zsh prompt variables

3. bash-compatible zsh script

## Benchmark

Startup time is tracked with a benchmark-driven workflow — measure, profile
with zprof, change one thing, re-measure. Full write-up with all optimization
techniques (compinit caching, fork elimination, deferred loading, zwc
compilation) lives in [PERFORMANCE.md](./PERFORMANCE.md).

```sh
# quick check (no dependencies)
./benchmark-shell-startup.zsh 10

# preferred (statistics + warmup)
hyperfine --warmup 3 'zsh -i -c exit' 'zsh -l -i -c exit'
```

| Scenario | Mean |
|----------|------|
| `zsh -i` (tmux pane) | ~40ms |
| `zsh -l -i` (new terminal) | ~48ms |

## Directory Structure

```
zsh/
├── autoloaded/       # Functions autoloaded by zsh (in fpath)
├── plugins/          # Plugin files loaded at startup
├── vendor/           # Third-party plugins and completions
├── widgets/          # ZLE widgets (*.widget.zsh)
├── PERFORMANCE.md    # Startup optimization notes and benchmarks
└── benchmark-*.zsh   # Benchmark scripts
```

## File Loading Order

The `~/.zshrc` loads components in this order:

1. **Environment setup** - `ZSHDIR`, paths
2. **Plugins** (`~/.config/zsh/plugins/*.plugin.zsh`) - Loaded early
3. **Completion system** (`compinit`) - Loaded after plugins
4. **Additional configs** (fzf, themes, etc.)

### Critical: Plugin Loading Order

Plugins are loaded **before** `compinit`. This means:

- ✅ **OK**: Define functions, aliases, variables in plugins
- ❌ **NOT OK**: Use `compdef` in plugin files (it's not available yet)
- ✅ **OK**: Put completion files in `autoloaded/` directory (fpath)

### Completion System

Zsh completion works via `fpath` - a list of directories where completion files are stored.

```zsh
# In ~/.zshrc
fpath=($ZSHDIR/autoloaded $HOME/.config/zfunc "${fpath[@]}")
autoload -Uz compinit
compinit -C   # fast path; full rebuild + touch once per 24h (see PERFORMANCE.md)
```

**Two ways to add completions:**

| Method | Location | Format | When to use |
|--------|----------|--------|-------------|
| `#compdef` directive | `autoloaded/` | `#compdef cmdname` | Preferred - works with compinit |
| Inline `compdef` | Plugin file | `compdef _func func` | ❌ Fails - compdef not defined yet |

**Example - Correct completion file** (`autoloaded/_tmuxrawcapture`):

```zsh
#compdef tmuxrawcapture

_tmuxrawcapture() {
  local -a sessions
  sessions=(${(f)"$(tmux list-sessions -F "#{session_name}" 2>/dev/null)"})
  if [[ $#sessions -gt 0 ]]; then
    _describe 'tmux sessions' sessions
  else
    _message 'no tmux sessions'
  fi
}
```

**Common pitfalls:**

1. Using `compdef` in plugin files → "command not found: compdef"
2. Forgetting `#compdef` directive → completion not registered
3. Not using `local -a` for arrays → completion shows as single string

### Adding New Completions

1. Create file in `autoloaded/_cmdname`
2. Add `#compdef cmdname` shebang
3. Define `_cmdname()` function with completion logic
4. Reinitialize: `rm -f ~/.zcompdump* && exec zsh`

**Note:** `which _func` shows `# undefined` - this is normal. Functions are lazy-loaded via `autoload -X` to speed up shell startup.
