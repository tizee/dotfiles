# macOS Utilities Collection

A collection of useful command-line utilities and scripts for macOS daily development and system management.

## Scripts Overview

### 🍺 **brewsm** - Brew Services Manager
A user-friendly wrapper for Homebrew services with enhanced visual feedback and comprehensive management options.

**Usage:**
```bash
brewsm [command] [service] [options]
```

**Commands:**
- `ls/list` - List all services with colored status indicators
- `start <service>` - Start and register a service
- `stop <service>` - Stop and unregister a service  
- `restart <service>` - Restart a service
- `run <service>` - Run service without registering
- `kill <service>` - Stop service but keep registered
- `info <service>` - Show detailed service information
- `cleanup` - Remove unused services

**Options:**
- `--all` - Apply to all services
- `--sudo` - Run with system privileges
- `--json` - JSON output format
- `--debug` - Show debugging info

**Examples:**
```bash
brewsm ls                    # List all services
brewsm start mysql          # Start MySQL service
brewsm restart --all        # Restart all services
brewsm stop nginx --sudo    # Stop Nginx with sudo
```

### 🌐 **ccproxy** - Claude Code Proxy
Configures Claude Code to use proxy settings from a JSON configuration file.

**Usage:**
```bash
ccproxy [claude-args...]
```

**Configuration:**
- Default config: `~/.config/llm/cc-proxy.json`
- Requires `auth_key` and `base_url` fields

**Example:**
```bash
ccproxy --help              # Run Claude with proxy settings
ccproxy -c custom.json      # Use custom config file
```

### 🐍 **conda_active** - Conda Environment Activator
Shell script to initialize Anaconda/Miniconda environment in zsh.

**Usage:**
```bash
source conda_active          # Activate conda in current shell
```

### 🔢 **cpunum** - CPU Core Counter
Simple utility to display the number of CPU cores on your Mac.

**Usage:**
```bash
cpunum                      # Returns number of CPU cores
```

### 🐛 **debugmac** - macOS Debug Mode Toggle
Enable or disable macOS system debugging features for web development.

**Usage:**
```bash
debugmac on                 # Enable debugging
debugmac off                # Disable debugging
```

**Features:**
- Enables WebKit Developer Tools in system web views
- Shows debug menu bar icon
- Enables context menus in web views

### 📝 **edit-claude-mcp** - Claude Desktop Config Editor
Quick command to edit Claude Desktop's MCP configuration file.

**Usage:**
```bash
edit-claude-mcp             # Opens config in default editor
```

### 📸 **exifinfo** - Image Metadata Extractor
Extract and display EXIF metadata from image files using exiftool or mdls as fallback.

**Usage:**
```bash
exifinfo image1.jpg [image2.png ...]
```

**Features:**
- Uses exiftool for comprehensive metadata
- Falls back to macOS mdls if exiftool unavailable
- Processes multiple files at once
- Shows detailed EXIF, GPS, and camera information

### 🔍 **find-my-project** - Git Project Finder
Find git repositories created by the current user in specified directories.

**Usage:**
```bash
find-my-project '~/projects/**/*(/F)'
```

**Features:**
- Searches directories for git repositories
- Checks if current user is the repository creator
- Uses zsh glob patterns for directory specification

### 🖼️ **heic2jpg** - HEIC to JPEG Converter
Convert HEIC images to JPEG format using macOS built-in sips tool.

**Usage:**
```bash
heic2jpg input.heic output.jpg
```

### 📋 **lsregister** - Launch Services Register
Wrapper for macOS Launch Services registration tool to manage URL schemes and application bindings.

**Usage:**
```bash
lsregister -dump URLSchemeBinding    # List all URL schemes
```

### 🧹 **maccleanup** - System Cleanup Utility
Comprehensive cleanup tool for development environments and system caches.

**Usage:**
```bash
maccleanup [option]
```

**Options:**
- `-zig [-clean]` - Clean Zig cache directories
- `-rust [-clean]` - Clean Rust target directories  
- `-go [-clean]` - Clean Go module cache
- `-node_modules [-clean]` - Clean Node.js dependencies
- `-homebrew` - Clean Homebrew packages and cache
- `-gem` - Clean outdated Ruby gems
- `-pod` - Clean CocoaPods cache
- `-simulator` - Remove unavailable Xcode simulators
- `-xcode` - Clean Xcode artifacts (Archives, DerivedData, Logs)
- `-all` - Run all cleanup operations

**Examples:**
```bash
maccleanup -node_modules       # Analyze node_modules
maccleanup -node_modules -clean # Actually clean node_modules
maccleanup -all                # Full system cleanup
```

### 🌐 **macproxy** - macOS Proxy Manager
Manage system-level proxy settings with support for environment variables.

**Usage:**
```bash
macproxy [command]
```

**Commands:**
- `status` - Show current proxy settings
- `on` - Enable proxy using environment variables
- `off` - Disable all proxy settings
- `env` - Show environment variable settings

**Environment Variables:**
- `http_proxy` - HTTP proxy URL
- `https_proxy` - HTTPS proxy URL  
- `socks_proxy` - SOCKS proxy URL
- `all_proxy` - Proxy for all protocols

**Examples:**
```bash
macproxy status              # Check current settings
macproxy on                  # Enable proxy
http_proxy=http://127.0.0.1:7890 macproxy on
```

### 🔍 **s** - Smart Search Launcher
Universal search launcher that opens search queries in default browser using configurable search engines.

**Usage:**
```bash
s [search-engine] <query>
```

**Features:**
- Configurable search engines via `~/.config/search_engines.json`
- Supports multiple search engines (Google, DuckDuckGo, GitHub, etc.)
- Caches search engine configurations
- Platform-aware URL opening

**Examples:**
```bash
s python tutorial              # Search with default engine
s g python tutorial           # Search with Google
s gh python repo              # Search GitHub repositories
```

### 🖥️ **setup_hostname** - System Hostname Configurator
Interactive tool to set macOS hostname, computer name, and local hostname.

**Usage:**
```bash
setup_hostname
```

**Settings:**
- `HostName` - Terminal and network identifier
- `ComputerName` - GUI display name
- `LocalHostName` - Bonjour and local network name

### 📍 **showip** - IP Address Display
Show the current IP address of the primary network interface.

**Usage:**
```bash
showip                       # Shows current IP address
```

### 🔐 **signxvim** - Xcode Code Signing for XVim2
Re-signs Xcode application for XVim2 plugin compatibility.

**Usage:**
```bash
signxvim                     # Re-signs /Applications/Xcode.app
```

### ⌨️ **update_skhd** - SKHD Hotkey Daemon Updater
Update skhd (Simple Hotkey Daemon) to the latest version using Homebrew.

**Usage:**
```bash
update_skhd                  # Updates skhd via Homebrew
```

### 🖥️ **upgrade_wezterm** - WezTerm Updater
Update WezTerm terminal emulator to the latest version.

**Usage:**
```bash
upgrade_wezterm              # Updates WezTerm via Homebrew
```

### 🔗 **vless2xray** - VLESS to Xray Config Converter
Convert VLESS proxy links to Xray outbound configuration format.

**Usage:**
```bash
vless2xray --tag <name> <vless-link>
```

**Example:**
```bash
vless2xray --tag proxy "vless://uuid@host:port?security=tls&type=ws&path=/ws#remark"
```

**Features:**
- Supports VLESS with various transport protocols (TCP, WebSocket, gRPC, HTTP)
- Handles TLS and Reality security configurations
- Generates complete Xray JSON configuration

### 📶 **wifipwd** - WiFi Password Retriever
Retrieve saved WiFi passwords from macOS keychain.

**Usage:**
```bash
wifipwd <SSID>                # Get password for specific network
```

## Installation

All scripts are executable and ready to use. Place them in your PATH or create symbolic links:

```bash
# Example: Add to PATH via ~/.zshrc
export PATH="/path/to/mac_scripts:$PATH"

# Or create symlinks
ln -s /path/to/mac_scripts/s /usr/local/bin/s
```

## Dependencies

Most scripts are self-contained, but some require:
- **Homebrew** (for package management scripts)
- **exiftool** (for enhanced image metadata in exifinfo)
- **jq** (for JSON processing in ccproxy)
- **Python 3** (for s and vless2xray)

Install missing dependencies:
```bash
brew install exiftool jq python3
```

## Configuration

- **Search engines**: Configure `~/.config/search_engines.json` for the `s` command
- **Proxy settings**: Configure `~/.config/llm/cc-proxy.json` for ccproxy
- **Cleanup settings**: Modify `DEFAULT_AGE_DAYS` in maccleanup for cache retention

## License

These scripts are provided as-is for personal use. Review each script before use to ensure they meet your security requirements.