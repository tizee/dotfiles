# hammerspoon modules

- `config.lua`
    - Configuration for mode layer components with human-readable action names
- `init.lua`
    - hammerspoon entry point
- `mode.lua`
    - hammerspoon mode layer component with support for display names
- `pasteboard-link-rewriter.lua`
    - rewrite pasteboard URL
- `quick-action-mode.lua`
    - a quick action mode layer with app toggles (Claude, Ghostty, Obsidian, Cherry Studio)
- `safe-command-q.lua`
    - confirm to quit for `⌘-q`
- `status-message.lua`
    - A message widget other than hammerspoon's alert modal.
- `switch-window-mode.lua`
    - **Performance optimized** window switcher for current app (`Ctrl+Tab`)
    - Fast window enumeration without slow space-aware filters
    - Filters out invalid windows (no title, minimized, invisible)
- `window-layout-mode.lua`
    - A window manager replacement. Since `yabai` has messed up the screen, I've decided to manage windows manually.
- `keyboard-mode.lua`
    - Switch input source (IME) with readable language names

## Key Features

### Quick Actions (`Ctrl+T`)
- `q` - Quick Navigation (window hints)
- `p` - Paste as Plain Text
- `a` - Show App Info
- `d` - Toggle Dark Mode
- `r` - Reload Config
- `y` - YouTube URL Cleaner
- `t` - Remove URL Trackers
- `h` - Toggle Claude
- `j` - Toggle Ghostty
- `k` - Toggle Obsidian
- `i` - Toggle Cherry Studio

### Input Source Switching (`Ctrl+S`)
- `i` - Show Current Input Source
- `h` - Traditional Chinese
- `j` - English
- `k` - Simplified Chinese
- `l` - Japanese Hiragana
- `o` - Japanese Katakana

### Window Switching (`Ctrl+Tab`)
- Fast app window switcher with performance optimizations
- Numbers 1-9 to switch between windows of current app
