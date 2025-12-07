# Claude Code Enhanced Hooks

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Lightweight JSON Architecture](#lightweight-json-architecture)
- [Installation and Configuration](#installation-and-configuration)
- [Log File Structure](#log-file-structure)
- [Usage Examples](#usage-examples)
- [Troubleshooting Guide](#troubleshooting-guide)
- [Data API Reference](#data-api-reference)
- [Advanced Usage](#advanced-usage)
- [Security Considerations](#security-considerations)

## Overview

This enhanced hook system provides deep analysis and performance tracking for Claude Code sessions.

## Features

### Bash Logger Hook (PostToolUse)
- **Command Logging**: Logs all Bash command executions to `/tmp/claude-bash.log`
- **Full Output Preservation**: Saves stdout and stderr to avoid losing truncated information
- **Auto Rotation**: 500KB/500 lines limit prevents unlimited log growth
- **Debug Aid**: View complete output via log file when output is truncated

### Stop Hook
- **Session Statistics**: Counts total tool usage, bash commands, file operations
- **Success Rate Analysis**: Calculates operation success rate automatically
- **Todo Status Check**: Detects incomplete todo items
- **Session Summary**: Generates session performance summary report

### SubagentStop Hook
- **Task Completion Analysis**: Analyzes completed, in-progress, and failed task states
- **Tool Usage Density**: Analyzes tool density per subtask
- **Performance Metrics Storage**: Stores subtask run data to JSON
- **Efficiency Assessment**: Estimates overall task processing efficiency

### PreCompact Hook
- **Compaction Impact Analysis**: Estimates size changes before/after compaction
- **Key Insight Preservation**: Identifies important keywords for priority retention
- **Compaction History Tracking**: Records detailed compaction info and recommendations
- **File Cleanup Recommendations**: Provides intelligent cleanup suggestions

## Lightweight JSON Architecture

### Storage Format Comparison
- **SQLite**: ❌ Heavy, requires database engine
- **JSON**: ✅ Lightweight, directly readable, native Python support
- **CSV**: ❌ Requires additional parsing libraries

### JSON Storage Structure

#### subagent-metrics.json
```json
[
  {
    "timestamp": "2024-07-13T10:30:00.000000",
    "session_id": "abc123",
    "stats": {
      "transcript_lines": 1523,
      "total_tools": 45,
      "success_count": 42,
      "error_count": 3,
      "bash_commands": 12,
      "file_operations": 15
    }
  }
]
```

#### cleanup-recommendations.json
```json
[
  {
    "timestamp": "2024-07-13T10:30:00.000000",
    "trigger_type": "manual",
    "insights_to_preserve": ["error", "success", "warning"],
    "file_metrics": {
      "lines": 1523,
      "bytes": 48392,
      "estimated_reduction": 8500
    }
  }
]
```

## Installation and Configuration

### 1. Permission Setup
Ensure all scripts have execute permissions:
```bash
chmod +x ~/.claude/hooks/*.py
```

### 2. Configuration Validation
JSON file size control:
- Each metrics file keeps maximum 100 records
- Automatically discards oldest data to prevent bloat
- Pure JSON architecture, no external dependencies

### 3. Claude Code Configuration

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/validate-commands.py"
          }
        ]
      },
      {
        "matcher": "Write|Edit|MultiEdit|Read",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/validate-files.py"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/operation-logger.py"
          }
        ]
      },
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/bash-logger.py"
          }
        ]
      }
    ],
    "Stop": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/stop-handler.py"
          }
        ]
      }
    ],
    "SubagentStop": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/subagent-stop-handler.py"
          }
        ]
      }
    ],
    "PreCompact": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/pre-compact-handler.py"
          }
        ]
      }
    ],
    "Notification": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "python3 ~/.claude/hooks/notification-handler.py"
          }
        ]
      }
    ]
  }
}
```

## Log File Structure

### ~/.claude/logs/ Directory
- `session-summary.log` - Session summary for each session
- `task-analysis.log` - Detailed subtask analysis
- `compaction-tracking.log` - Detailed compaction operation records
- `cleanup-recommendations.json` - Intelligent cleanup recommendations
- `subagent-metrics.json` - Lightweight performance metrics JSON
- `operations.log` - File operation records (Write/Edit/MultiEdit)

### /tmp/ Directory
- `claude-bash.log` - Bash command execution log (with full stdout/stderr)

## Usage Examples

### View Truncated Bash Output
When command output is truncated, view the complete log:
```bash
# View recent command execution records
tail -100 /tmp/claude-bash.log

# Search for specific command output
grep -A 20 "npm run build" /tmp/claude-bash.log
```

### Post-Session Completion Check
Auto-check at session end:
- Tool usage statistics
- Incomplete TodoWrite items reminder
- Operation success rate and error statistics

**Output Example**:
```
[2024-07-13T15:45:12.123456] Session Summary:
  Total Tools: 23
  Bash Commands: 8
  File Operations: 12
  Success Rate: 95.7%
  Warnings: 2
  Errors: 1
  Warning: 2 todo items are still incomplete
```

### Performance Trend Analysis
View historical session metrics:
```bash
# View raw JSON data
cat ~/.claude/logs/subagent-metrics.json | jq -r '.[-5:][] | {timestamp, stats}'

# Analyze tool usage trends
jq '.[] | [.timestamp, .stats.total_tools, .stats.success_count]' ~/.claude/logs/subagent-metrics.json
```

### Pre-Compaction Analysis
Get cleanup suggestions before compaction:
```bash
# View compaction recommendations
jq '.[-1]' ~/.claude/logs/cleanup-recommendations.json
```

## Troubleshooting Guide

### Daily Check Commands
```bash
# Verify script permissions
ls -la ~/.claude/hooks/*.py
chmod +x ~/.claude/hooks/*.py

# View real-time logs
tail -f ~/.claude/logs/session-summary.log

# View performance statistics
jq length ~/.claude/logs/subagent-metrics.json
```

### Common Issues

#### Q: How to debug Stop hook errors?
```bash
# Manually test hook
echo '{"transcript_path": "/tmp/test.json", "session_id": "test"}' | python3 ~/.claude/hooks/stop-handler.py
```

#### Q: JSON file too large?
- Auto-limited to 100 records (built-in)
- Can directly delete history files
- Won't cause system performance issues

#### Q: TodoWrite statistics inaccurate?
- TodoWrite is an independent tool, unrelated to Task
- Reads directly from transcript file, no Task dependency
- Tracks pending/in_progress/completed states simultaneously

## Data API Reference

### Session Statistics Fields
```json
{
  "total_tools": "Total tools used",
  "bash_commands": "Bash commands executed",
  "file_operations": "File operation count (R/W/E/ME)",
  "success_count": "Successful operations",
  "error_count": "Failed operations",
  "todo_incomplete": "Incomplete TodoWrite items"
}
```

### Quick Command Toolbox
```bash
# View current session summary
alias claude-stats="tail -n 10 ~/.claude/logs/session-summary.log"

# Check todo status
alias todo-watch="grep -c 'pending\|in_progress' ~/.claude/logs/session-summary.log"

# Generate daily usage report
function claude-report() {
  echo "=== Claude Code Daily Report ==="
  echo "Sessions completed:" $(wc -l < ~/.claude/logs/session-summary.log)
  echo "Total tools used:" $(jq '.[] | .stats.total_tools' ~/.claude/logs/subagent-metrics.json | jq -s add)
}
```

## Advanced Usage

### Shell Prompt Integration
```bash
# Display current session tool count statistics
function claude_session_info() {
  local stats_file="$HOME/.claude/logs/session-summary-simple.log"
  if [[ -f "$stats_file" ]]; then
    tail -1 "$stats_file" | grep -o 'Session Summary.*$' || echo "Ready for session"
  fi
}
```

### TodoWrite Completion Report
```bash
#!/bin/bash
# Generate daily TodoWrite completion report
logfile="~/.claude/logs/session-summary.log"
if [[ -f $logfile ]]; then
    echo "=== TodoWrite Daily Report ==="
    grep "todo items" "$logfile" | tail -5
fi
```

## Security Considerations

- **Zero System Permissions**: Only operates in user home directory
- **Safe to Delete**: JSON files can be deleted directly to reset
- **Read-Only Operations**: Does not modify other system files except logs
- **Capacity Control**: Intelligent file size limits prevent bloat

All hooks read and write log files within the current user environment and do not perform destructive operations. All file paths are validated and permission-checked.

## Troubleshooting

If hooks don't work:
1. Check JSON configuration is correct
2. Confirm all scripts have execute permissions
3. Check `~/.claude/logs/` for error logs
4. Use `/hooks` command to verify configuration
