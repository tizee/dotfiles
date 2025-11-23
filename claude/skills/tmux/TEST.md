# Testing the tmux-enhanced Skill

This document describes how to test the tmux-enhanced skill to verify it works correctly.

## Test Suite

A comprehensive test script is provided to validate all functionality:

### Quick Test

Run the complete test suite:

```bash
cd ~/.config/claude/skills/tmux
tools/test-skill.sh
```

### Test Options

The test suite supports several output modes:

```bash
tools/test-skill.sh              # Run all tests with normal output
tools/test-skill.sh -v           # Run with verbose DEBUG output
tools/test-skill.sh -q           # Run quietly (pass/fail only)
tools/test-skill.sh -h           # Show help
```

### Manual Testing

If you prefer to test manually:

#### Test 1: Dependency Check
```bash
# Check tmux installation
command -v tmux && echo "tmux $(tmux -V) found" || echo "tmux not found"
```

#### Test 2: Create Clean Session
```bash
# Create test socket directory
mkdir -p "${TMPDIR:-/tmp}/claude-tmux-sockets"

# Create clean session (isolated config)
tools/create-session.sh \
  -n test-clean \
  -c clean \
  -p 'echo "test"' \
  -w 'test' \
  -t 5

# Verify session exists
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/test-clean.sock" list-sessions

# Clean up
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/test-clean.sock" kill-session -t test-clean
```

#### Test 3: Create User Session
```bash
# Create user session with your config
tools/create-session.sh \
  -n test-user \
  -c user \
  -p 'echo "test"' \
  -w 'test' \
  -t 5

# Verify session exists and has your keybindings
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/test-user.sock" show-option -g prefix
# Should show: prefix C-a (or your custom prefix)

# Clean up
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/test-user.sock" kill-session -t test-user
```

#### Test 4: Find Sessions
```bash
# List all sessions
tools/find-sessions.sh --all

# Filter by name
tools/find-sessions.sh -q "python"

# Check specific socket
tools/find-sessions.sh -s "${TMPDIR:-/tmp}/claude-tmux-sockets/test-clean.sock"
```

#### Test 5: Python REPL Session
```bash
# Test Python REPL with performance optimization
tools/create-session.sh \
  -n python-test \
  -c clean \
  -p 'PYTHON_BASIC_REPL=1 python3 -q' \
  -w '^>>>' \
  -t 10

# Send Python commands
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/python-test.sock" send-keys -t "python-test:0.0" "print('Hello, World!')" Enter

# Capture output
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/python-test.sock" capture-pane -p -t "python-test:0.0" -S -5

# Should show: Hello, World!

# Clean up
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/python-test.sock" kill-session -t python-test
```

#### Test 6: LLDB Session (if available)
```bash
# Create a simple program to debug
echo '#include <stdio.h>
int main() { printf("test"); return 0; }' > /tmp/test.c
clang -g /tmp/test.c -o /tmp/test-program

# Start LLDB session with lldbinit support
tools/create-session.sh \
  -n lldb-test \
  -c user \
  -p 'lldb /tmp/test-program' \
  -w '\\(lldbinit\\)' \
  -t 10

# Send LLDB commands
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/lldb-test.sock" send-keys -t "lldb-test:0.0" "breakpoint set -n main" Enter

# Clean up
tmux -S "${TMPDIR:-/tmp}/claude-tmux-sockets/lldb-test.sock" kill-session -t lldb-test
rm -f /tmp/test.c /tmp/test-program
```

### Test Suite Details

The automated test suite runs 8 tests:

1. **Test 1**: Dependency Check - Verifies tmux is installed and accessible
2. **Test 2**: Create Clean Session - Tests isolated session creation
3. **Test 3**: Create User Session - Tests user config session creation
4. **Test 4**: Find Sessions - Tests session discovery functionality
5. **Test 5**: Wait for Text - Verifies session responsiveness
6. **Test 6**: Python REPL - Tests Python REPL integration
7. **Test 7**: LLDB Session - Tests LLDB debugger integration with lldbinit
8. **Test 8**: Cleanup - Verifies manual session cleanup

### Expected Test Results

When all tests pass, you should see:

```bash
Preparing test environment...
Cleaning up test artifacts...
Testing tmux-enhanced skill...

Test 1: Checking dependencies...
✓ Dependencies verified tmux 3.x

Test 2: Creating clean session...
✓ Clean session created

Test 3: Creating user session...
✓ User session created

Test 4: Finding sessions...
✓ test-skill-clean session found
✓ test-skill-user session found

Test 5: Wait for text...
✓ Session test-skill-clean is responding

Test 6: Testing Python REPL...
✓ Python REPL working correctly

Test 7: Testing LLDB with lldbinit...
✓ LLDB session created

Test 8: Testing cleanup...
✓ Cleanup successful

========================================

All tests passed! ✓

Summary: 8 tests passed
The tmux-enhanced skill is working correctly.
Cleaning up test artifacts...
```

**Note**: The "Cleaning up test artifacts..." message at the end indicates cleanup completed successfully. Cleanup only runs when all tests pass, preserving logs for debugging when tests fail.

### Test Coverage

The test suite covers:

- **Dependencies**: tmux availability (bash version compatibility built-in)
- **Session Creation**: All config levels (clean, user)
- **Session Management**: Find and list sessions
- **Process Interaction**: Send keys and wait for output
- **Real-world Scenarios**: Python REPL, LLDB debugging with lldbinit
- **Cleanup**: Proper session termination and artifact cleanup
- **Error Handling**: Timeout conditions, missing dependencies

## Continuous Integration

To add this to CI/CD:

```yaml
# .github/workflows/test-tmux-skill.yml
name: Test tmux-enhanced Skill

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y tmux clang python3
      - name: Run tests
        run: |
          cd ~/.config/claude/skills/tmux
          bash tools/test-skill.sh
```

## Troubleshooting Test Failures

### "tmux not found"
**Solution**: Install tmux
```bash
# macOS
brew install tmux

# Ubuntu/Debian
sudo apt-get install tmux

# Fedora
sudo dnf install tmux
```

### Session creation timeout
**Solution**: Increase timeout value
```bash
tools/create-session.sh -n test -c clean -p 'python3' -w '>>>' -t 30  # 30 seconds
```

### Permission denied on socket
**Solution**: Check socket directory permissions
```bash
ls -la "${TMPDIR:-/tmp}/claude-tmux-sockets"
# Should be writable by current user
```

### User config not found
**Solution**: Create a minimal config or use clean/custom mode
```bash
# Either create config
mkdir -p "$HOME/.config/tmux"
touch "$HOME/.config/tmux/tmux.conf"

# Or use clean mode
tools/create-session.sh -n test -c clean -p 'bash'
```

### LLDB test fails on macOS
**Solution**: Install command line tools
```bash
xcode-select --install
```

### Python REPL test timeout
**Solution**: Make sure python3 is available
```bash
which python3 || echo "Install python3"
```

## Cleanup Behavior

The test suite has intelligent cleanup behavior:

- **Before tests**: Cleanup runs to ensure a clean test environment
- **After successful tests**: Cleanup removes all artifacts (sockets, logs, test files)
- **After failed tests**: Cleanup is skipped to preserve logs for debugging
- **On interrupt (Ctrl+C)**: Cleanup runs via trap handler

Logs are preserved in `/tmp/` when tests fail:
- `/tmp/test-clean.log` - Clean session test logs
- `/tmp/test-user.log` - User session test logs
- `/tmp/python-test.log` - Python REPL test logs
- `/tmp/lldb-test.log` - LLDB test logs

## Reporting Issues

If tests fail:

1. Run in verbose mode to see debug output:
   ```bash
   bash -x tools/test-skill.sh -v
   ```

2. Check test logs (if tests failed, they won't be cleaned up):
   ```bash
   ls -la /tmp/test-*.log /tmp/python-test.log /tmp/lldb-test.log
   cat /tmp/test-clean.log  # or other log files
   ```

3. Verify prerequisites:
   - `tmux -V` (need 3.0+)
   - `bash --version`
   - `uname -a`
   - `which python3`
   - `which lldb && which clang` (for LLDB test)

4. Capture full output and report with:
   - tmux version
   - bash version
   - OS details
   - Full error message
   - Relevant log file contents
