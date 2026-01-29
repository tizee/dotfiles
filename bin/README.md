# 🛠️ Personal Command Collection

A curated collection of useful command-line tools and utilities for daily development workflow. Each script is designed to solve specific problems efficiently.

## 📋 Command Reference

### 🎥 Media & Video Tools

#### `ascii`
**Purpose**: Enhanced wrapper for asciinema terminal recording
**Usage**: `ascii [asciinema-args]`
**Details**: Automatically creates recording directory if not exists, provides colored error messages for missing dependencies

#### `clip-ytb-video`
**Purpose**: Clip YouTube videos on-demand with precise timing
**Usage**: `clip-ytb-video <url> <start-time> <duration> <output-file>`
**Example**: `clip-ytb-video "https://youtube.com/watch?v=..." 00:01:30 30 clip.mp4`
**Details**: Uses yt-dlp to extract stream URLs and ffmpeg for clipping. Supports both video+audio and video-only streams

#### `clipvideo`
**Purpose**: Remove segments from local video files
**Usage**: `clipvideo <input.mp4> <start-seconds> <end-seconds> <output.mp4>`
**Example**: `clipvideo input.mp4 10 20 output.mp4`
**Details**: Uses ffmpeg to extract video segments by removing specified time ranges

### 🔍 Search & Navigation

#### `findr`
**Purpose**: Bottom-up file search traversing parent directories
**Usage**: `findr <path> <pattern>`
**Example**: `findr $(pwd) "*.py"`
**Details**: Recursively searches upward through parent directories until root, uses `fd` if available, falls back to `find`

#### `pfzf`
**Purpose**: Interactive file selection with fzf and code analysis
**Usage**: `pfzf <codebase-directory> [depth]`
**Example**: `pfzf ~/projects/myapp 2`
**Details**: Uses fzf for interactive file selection with syntax highlighting preview via bat, runs code2prompt on selected files

#### `search_tmux_binding`
**Purpose**: Search tmux key bindings by table and key
**Usage**: `search_tmux_binding <table> <key>`
**Example**: `search_tmux_binding prefix I`
**Details**: Searches tmux key bindings in specified tables (prefix, root, session, window) for specific key combinations

### 🔐 Security & Encoding

#### `bashfuck`
**Purpose**: Generate obfuscated bash payloads for command execution
**Usage**: Interactive - run `bashfuck` and enter commands
**Details**: Creates multiple forms of bash command obfuscation using octal encoding, arithmetic expansion, and parameter substitution. Useful for bypassing basic filters

#### `caesar`
**Purpose**: Intelligent Caesar cipher tool with ROT13 support
**Usage**: `caesar [text] -n <shift> [-d] [-i input] [-o output]`
**Example**: `caesar "hello" -n 3` → "khoor"
**Details**: Supports encryption/decryption, file I/O, pipe input, automatic ROT13 handling (shift 13), and verbose output

#### `gentoken`
**Purpose**: Generate cryptographically secure OAuth tokens
**Usage**: `gentoken [--prefix sk] [--length 32] [--count 1] [--timestamp]`
**Example**: `gentoken --prefix api --count 5`
**Details**: Creates secure random tokens with customizable prefixes, lengths, and optional timestamps for API keys, secrets, etc.

#### `uriencoder`
**Purpose**: URL encode strings for web applications
**Usage**: `uriencoder "string to encode"` or pipe input
**Example**: `uriencoder "hello world"` → "hello%20world"
**Details**: Uses Python's urllib.parse for proper URL encoding of special characters

### 🌐 Network & IP Tools

#### `ip4to6`
**Purpose**: Convert between IPv4 and IPv6 addresses
**Usage**: `ip4to6 <ip-address>`
**Example**: `ip4to6 192.168.1.1`
**Details**: Shows IPv4 to IPv6 mapping and identifies IPv6-only addresses

#### `machineid`
**Purpose**: Get unique machine identifier across platforms
**Usage**: `machineid`
**Details**: Retrieves system UUID from platform-specific sources (ioreg on macOS, /var/lib/dbus/machine-id on Linux, /etc/hostid on BSD)

#### `myip`
**Purpose**: Display local and public IP addresses
**Usage**: `myip [target] [options]`
**Targets**:
- `local`, `l` - Show only local network IP
- `public`, `p` - Show only public IP
- `all`, `a` - Show both (default)
**Options**:
- `-print0`, `-0` - Output raw IP only (no formatting, no newline)
- `-c`, `--copy` - Copy IP to clipboard
**Examples**:
- `myip` - Show all IPs (formatted)
- `myip local -print0` - Output local IP raw (for scripting)
- `myip public -c` - Copy public IP to clipboard
**Details**: Cross-platform (macOS/Linux). Shows all active interfaces (Wi-Fi, Ethernet, VPN/Tailscale). Uses multiple fallback services for public IP detection (ipify, ifconfig.me, icanhazip)

### 🔧 Development Tools

#### `dart_lsp_server`
**Purpose**: Launch Dart Language Server Protocol server
**Usage**: `dart_lsp_server`
**Details**: Starts the Dart analysis server in LSP mode for editor integration

#### `git-sview`
**Purpose**: Git repository statistics and visualization
**Usage**: `git-sview`
**Details**: Uses zx script to provide enhanced git repository insights (see zx_scripts/git-sview.mjs)

### 🤖 AI/LLM Tools

#### `llm-update`
**Purpose**: Interactive TUI for updating AI CLI tools and LLM agents
**Usage**: `llm-update`
**Features**:
- Beautiful terminal UI with color-coded status indicators
- Automatic dependency detection (checks for pnpm/uv availability)
- Smart tool selection (space-separated numbers or Enter for all)
- Pre-update confirmation step
- Real-time progress feedback with visual indicators
- Comprehensive summary with success/failure reporting

**Supported Tools**:
- `claude` - Claude Code CLI (requires pnpm)
  - Package: `@anthropic-ai/claude-code`
  - Install: `pnpm add -g @anthropic-ai/claude-code@latest`
- `codex` - OpenAI Codex CLI (requires pnpm)
  - Package: `@openai/codex`
  - Install: `pnpm add -g @openai/codex@latest`
- `kimi-cli` - Kimi CLI (requires uv)
  - Package: `kimi-cli`
  - Install: `uv tool install kimi-cli` or `uv tool upgrade --no-cache kimi-cli`
- `llm` - LLM CLI by Simon Willison (requires uv)
  - Package: `llm`
  - Install: `uv tool install llm` or `uv tool upgrade --no-cache llm`

**Details**: Provides a professional TUI experience for managing multiple LLM tools with box-drawing characters, status icons (✓/✗), and colored output. Automatically skips tools with missing dependencies.

**Note**: If updates fail, verify the package names above are still correct. Package names may change over time.

#### `claude-quota`
**Purpose**: Claude quota refresh daemon - keeps Claude Code quota window active
**Usage**:
- `claude-quota start [--time HH:MM]` - Start daemon (default: continuous mode)
- `claude-quota start --interval 1h30m` - Start with interval mode
- `claude-quota stop` - Stop daemon
- `claude-quota status` - Check status
- `claude-quota schedule HH:MM` - Update schedule (scheduled mode)
- `claude-quota interval 1h30m` - Set interval (interval mode)
- `claude-quota continue` - Switch to continuous mode

**Three modes**:
1. **Continuous mode** (default): Healthcheck ping every 30 minutes at :00 and :30 marks
2. **Scheduled mode**: Ping at specified time, then auto-schedule next ping 5h later (aligns with Claude Code's sliding window quota reset)
3. **Interval mode**: Ping at regular intervals (e.g., every 1h30m)

**Time format**: 24-hour format (HH:MM, e.g., 08:00, 20:30) or duration (e.g., 1h, 30m, 45s)
**Duration format**: XhYmZs (e.g., 1h30m, 45m, 2h, 90s)

**Examples**:
- `claude-quota start --time 08:00` - Start with scheduled mode at 8:00 AM
- `claude-quota start --interval 1h30m` - Start with interval mode every 1.5 hours
- `claude-quota schedule 1h` - Schedule next ping 1 hour from now
- `claude-quota interval 45m` - Set interval to 45 minutes
- `claude-quota status` - Show daemon status in systemctl-style format

**Details**: Daemon runs in background using minimal Haiku tokens to ping Claude Code, keeping quota window active. Uses PID file tracking and signal-based wake-up for dynamic mode changes. Logs to `~/.claude-quota/daemon.log`.

### 📁 File Management

#### `catcopy`
**Purpose**: Copy file contents or stdin to clipboard
**Usage**: `catcopy [file1 file2 ...]` or pipe input
**Example**: `cat file.txt | catcopy` or `catcopy *.txt`
**Details**: Concatenates multiple files or stdin and copies to system clipboard

#### `mark`
**Purpose**: Vim-like directory bookmarking system
**Usage**:
- `mark +name` - save current directory as bookmark
- `mark name` - navigate to bookmark
- `mark -name` - delete bookmark
- `mark` - list all bookmarks
**Details**: Uses symbolic links in ~/.local/share/marks for persistent bookmarks across sessions

#### `cpath`
**Purpose**: Copy absolute path of file or directory to clipboard
**Usage**: `cpath <file-or-directory> [...]`
**Example**:
- `cpath file.txt` - Copy single file path
- `cpath file1.txt file2.txt` - Copy multiple paths (one per line)
**Details**: Resolves relative paths to absolute paths using `realpath`, prints path(s) to stdout and copies to clipboard

#### `open`
**Purpose**: Cross-platform file opener
**Usage**: `open <file-or-directory>`
**Details**: Uses appropriate opener for platform (macOS: open, WSL: explorer.exe)

### 📋 Clipboard Utilities

#### `pbcopy`
**Purpose**: Cross-platform clipboard copy
**Usage**: `pbcopy` (pipe input) or `pbcopy [args]`
**Details**: Works on macOS (native pbcopy) and Linux (xclip fallback)

#### `pbpaste`
**Purpose**: Cross-platform clipboard paste
**Usage**: `pbpaste`
**Details**: Works on macOS (native pbpaste) and Linux (xclip fallback)

#### `osccp`
**Purpose**: Copy files via terminal OSC52 sequence
**Usage**: `osccp <file>`
**Details**: Uses terminal escape sequences for clipboard operations, useful over SSH

### 📊 Utility Tools

#### `num2str`
**Purpose**: Convert between binary/hex strings and character encodings
**Usage**: `num2str <encoding> <binary|hex> [string]` or `num2str -<encoding> <binary|hex> [string]`
**Example**: `num2str gbk binary 1100010011100011`
**Details**: Supports multiple encodings (utf-8, gbk, cp936, etc.) for binary/hex string conversion

#### `num2zh`
**Purpose**: Convert Arabic numerals to Traditional Chinese
**Usage**: `num2zh <number>`
**Example**: `num2zh 1234` → "壹仟贰佰叁拾肆"
**Details**: Converts numbers to traditional Chinese financial numerals used in formal documents

#### `temperature`
**Purpose**: Temperature conversion between Celsius and Fahrenheit
**Usage**: `temperature [-c] <fahrenheit>` or `temperature [-f] <celsius>`
**Example**: `temperature 32` → "0.00" (F to C)
**Details**: Uses bc for precise decimal calculations with emoji indicators

#### `truecolor`
**Purpose**: Test terminal true color support
**Usage**: `truecolor`
**Details**: Displays a color gradient to verify 24-bit color support in terminal

#### `unix`
**Purpose**: Display Unix philosophy ASCII art
**Usage**: `unix`
**Details**: Shows colorful ASCII art with the quote "UNIX IS VERY SIMPLE IT JUST NEEDS A GENIUS TO UNDERSTAND ITS SIMPLICITY"

#### `yearp`
**Purpose**: Display yearly progress bar
**Usage**: `yearp`
**Details**: Shows visual progress bar indicating how much of the current year has passed

### 🔗 Network Tunneling

#### `ggclonet`
**Purpose**: Git clone via SSH tunnel (deprecated)
**Usage**: `ggclonet <git-url> [clone-args]`
**Details**: Rewrites git URLs to use localhost SSH tunnel for bypassing network restrictions

#### `sshtunnel`
**Purpose**: Setup SSH tunnel for GitHub access
**Usage**: `sshtunnel`
**Details**: Creates local port forwarding from localhost:port to github.com:22 via VPS

## 🚀 Quick Start

Most scripts are self-contained and require no additional setup. Some may need:
- Python 3.7+ for Python-based tools
- External dependencies: ffmpeg, yt-dlp, fzf, bat, code2prompt
- Package managers: pnpm (for npm-based LLM tools), uv (for Python-based LLM tools)
- Platform-specific: xclip (Linux), pbcopy/pbpaste (macOS)

## 📁 Script Categories

- **Media Processing** (3): ascii, clip-ytb-video, clipvideo
- **Search & Navigation** (3): findr, pfzf, search_tmux_binding
- **Security & Encoding** (4): bashfuck, caesar, gentoken, uriencoder
- **Network Tools** (3): ip4to6, machineid, myip
- **Development** (2): dart_lsp_server, git-sview
- **AI/LLM Tools** (2): llm-update, claude-quota
- **File Management** (5): catcopy, mark, cpath, open, osccp
- **Clipboard** (3): pbcopy, pbpaste, osccp
- **Utilities** (7): num2str, num2zh, temperature, truecolor, unix, yearp
- **Network Tunneling** (2): ggclonet, sshtunnel
