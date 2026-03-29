# rtpm (rip-tpm)

Lightweight Python tmux plugin manager. Drop-in replacement for [tpm](https://github.com/tmux-plugins/tpm).

## Usage

Replace the tpm `run` line in your `tmux.conf`:

```tmux
# run '~/.config/tmux/plugins/tpm/tpm'
run '~/.config/tmux/plugins/rtpm/rtpm source'
```

Press `prefix + M-i` to open the interactive plugin manager popup:

```
  [+] tmux-resurrect  (tmux-plugins/tmux-resurrect)
  [+] tmux-osinfo     (tizee/tmux-osinfo)
  [-] tmux-new        (user/tmux-new)

  [i]nstall  [u]pdate  [c]lean  [q]uit
```

### Custom keybinding

```tmux
set -g @rtpm-key 'T'   # default: M-i
```

## CLI

```
rtpm install          # shallow-clone missing plugins
rtpm update [name]    # fetch --depth=1 + reset --hard FETCH_HEAD
rtpm clean            # remove plugins not in config
rtpm list             # show configured vs installed
rtpm menu             # interactive popup (install/update/clean)
rtpm source           # source *.tmux files (for tmux.conf)
```
