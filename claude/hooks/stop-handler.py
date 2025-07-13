#!/usr/bin/env python3
"""
Stop Hook Handler for Claude Code

This script handles the Stop event, which runs when the main Claude Code agent
has finished responding. It provides logging and can optionally block stopping
if certain conditions are met.
"""

import json
import re
import sys
from datetime import datetime, timezone
from pathlib import Path


def rotate_log_file(log_file, max_size_kb=512):
    """Rotate log file if it exceeds max_size_kb."""
    log_path = Path(log_file)
    if not log_path.exists():
        return

    try:
        file_size_kb = log_path.stat().st_size / 1024
        if file_size_kb > max_size_kb:
            backup_file = log_path.with_suffix(log_path.suffix + ".old")
            if backup_file.exists():
                backup_file.unlink()
            log_path.rename(backup_file)
    except Exception as e:
        print(f"Warning: Could not rotate log file {log_file}: {e}", file=sys.stderr)


def log_stop_event(data):
    """Log stop event to a file for debugging purposes."""
    log_file = Path.home() / ".claude" / "logs" / "stop-events.log"
    log_file.parent.mkdir(parents=True, exist_ok=True)

    # Rotate log file if it's too large
    rotate_log_file(log_file)

    with log_file.open("a") as f:
        timestamp = datetime.now(timezone.utc).isoformat()
        f.write(f"[{timestamp}] Stop Event:\n")
        f.write(json.dumps(data, indent=2))
        f.write("\n" + "=" * 50 + "\n")


def calculate_session_stats(transcript_path):
    """Calculate session statistics from the transcript."""
    transcript_file = Path(transcript_path) if transcript_path else None
    if not transcript_file or not transcript_file.exists():
        return None

    stats = {
        "total_tools": 0,
        "bash_commands": 0,
        "file_operations": 0,
        "success_rate": 0,
        "warnings": 0,
        "errors": 0,
    }

    try:
        with transcript_file.open() as f:
            content = f.read()

        # Count tool usages
        stats["total_tools"] = len(
            re.findall(r'"tool_name"\s*:\s*"[A-Za-z]+"', content)
        )
        stats["bash_commands"] = len(re.findall(r'"tool_name"\s*:\s*"Bash"', content))
        stats["file_operations"] = len(
            re.findall(r'"tool_name"\s*:\s*"(Read|Write|Edit|MultiEdit)"', content)
        )

        # Count outcomes
        total_ops = len(re.findall(r'"success"\s*:\s*', content))
        if total_ops > 0:
            successful_ops = len(re.findall(r'"success"\s*:\s*true', content))
            stats["success_rate"] = (successful_ops / total_ops) * 100

        stats["warnings"] = len(re.findall(r'"severity"\s*:\s*"warning"', content))
        stats["errors"] = len(re.findall(r'"severity"\s*:\s*"error"', content))

    except Exception:
        # Silently fail if we can't analyze transcript - this is intentional
        # to prevent analysis operations from interfering with main functionality
        pass

    return stats


def log_session_summary(stats):
    """Log session summary to a separate file."""
    log_file = Path.home() / ".claude" / "logs" / "session-summary.log"
    log_file.parent.mkdir(parents=True, exist_ok=True)

    with log_file.open("a") as f:
        timestamp = datetime.now(timezone.utc).isoformat()
        f.write(f"[{timestamp}] Session Summary:\n")
        f.write(f"  Total Tools: {stats['total_tools']}\n")
        f.write(f"  Bash Commands: {stats['bash_commands']}\n")
        f.write(f"  File Operations: {stats['file_operations']}\n")
        f.write(f"  Success Rate: {stats['success_rate']:.1f}%\n")
        f.write(f"  Warnings: {stats['warnings']}\n")
        f.write(f"  Errors: {stats['errors']}\n")
        f.write("-" * 40 + "\n")


def check_todo_status(transcript_path):
    """Check for incomplete TodoWrite items from transcript."""
    transcript_file = Path(transcript_path) if transcript_path else None
    if not transcript_file or not transcript_file.exists():
        return {"incomplete": 0, "total": 0}

    todo_pattern = (
        r'TodoWrite.*?(?:"status"\s*:\s*"in_progress"|"status"\s*:\s*"pending")'
    )

    try:
        with transcript_file.open() as f:
            content = f.read()

        pending_todos = len(re.findall(todo_pattern, content, re.IGNORECASE))
        total_todos = len(re.findall(r"TodoWrite", content))

    except Exception:
        return {"incomplete": 0, "total": 0}
    else:
        return {"incomplete": pending_todos, "total": total_todos}


def main():
    """Main handler for stop events."""
    try:
        # Read JSON input from stdin
        input_data = json.load(sys.stdin)

        # Log the event
        log_stop_event(input_data)

        # Check if stop hook is already active
        if input_data.get("stop_hook_active", False):
            # Prevent infinite loops
            print("Stop hook already active, allowing normal stop", file=sys.stderr)
            sys.exit(0)

        # Calculate session statistics
        session_stats = calculate_session_stats(input_data.get("transcript_path", ""))
        if session_stats:
            log_session_summary(session_stats)

        # Check for todo status (independent of Task tool)
        todo_status = check_todo_status(input_data.get("transcript_path", ""))
        if todo_status.get("incomplete", 0) > 0:
            print(
                f"Warning: {todo_status['incomplete']} todo items are still incomplete",
                file=sys.stderr,
            )

        # Exit with code 0 to allow normal stopping
        sys.exit(0)

    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error processing stop event: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
