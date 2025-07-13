#!/usr/bin/env python3
"""
Stop Hook Handler for Claude Code

This script handles the Stop event, which runs when the main Claude Code agent
has finished responding. It provides logging and can optionally block stopping
if certain conditions are met.
"""

import json
import sys
import os
import re
from datetime import datetime



def rotate_log_file(log_file, max_size_kb=512):
    """Rotate log file if it exceeds max_size_kb."""
    if not os.path.exists(log_file):
        return

    try:
        file_size_kb = os.path.getsize(log_file) / 1024
        if file_size_kb > max_size_kb:
            backup_file = f"{log_file}.old"
            if os.path.exists(backup_file):
                os.remove(backup_file)
            os.rename(log_file, backup_file)
    except Exception as e:
        print(f"Warning: Could not rotate log file {log_file}: {e}", file=sys.stderr)


def log_stop_event(data):
    """Log stop event to a file for debugging purposes."""
    log_file = os.path.expanduser("~/.claude/logs/stop-events.log")
    os.makedirs(os.path.dirname(log_file), exist_ok=True)

    # Rotate log file if it's too large
    rotate_log_file(log_file)

    with open(log_file, "a") as f:
        timestamp = datetime.now().isoformat()
        f.write(f"[{timestamp}] Stop Event:\n")
        f.write(json.dumps(data, indent=2))
        f.write("\n" + "="*50 + "\n")

def calculate_session_stats(transcript_path):
    """Calculate session statistics from the transcript."""
    if not transcript_path or not os.path.exists(transcript_path):
        return None

    stats = {
        'total_tools': 0,
        'bash_commands': 0,
        'file_operations': 0,
        'success_rate': 0,
        'warnings': 0,
        'errors': 0
    }

    try:
        with open(transcript_path, 'r') as f:
            content = f.read()

        # Count tool usages
        stats['total_tools'] = len(re.findall(r'"tool_name"\s*:\s*"[A-Za-z]+"', content))
        stats['bash_commands'] = len(re.findall(r'"tool_name"\s*:\s*"Bash"', content))
        stats['file_operations'] = len(re.findall(r'"tool_name"\s*:\s*"(Read|Write|Edit|MultiEdit)"', content))

        # Count outcomes
        total_ops = len(re.findall(r'"success"\s*:\s*', content))
        if total_ops > 0:
            successful_ops = len(re.findall(r'"success"\s*:\s*true', content))
            stats['success_rate'] = (successful_ops / total_ops) * 100

        stats['warnings'] = len(re.findall(r'"severity"\s*:\s*"warning"', content))
        stats['errors'] = len(re.findall(r'"severity"\s*:\s*"error"', content))

    except Exception:
        pass

    return stats


def log_session_summary(stats):
    """Log session summary to a separate file."""
    log_file = os.path.expanduser("~/.claude/logs/session-summary.log")
    os.makedirs(os.path.dirname(log_file), exist_ok=True)

    with open(log_file, "a") as f:
        timestamp = datetime.now().isoformat()
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
    if not transcript_path or not os.path.exists(transcript_path):
        return {"incomplete": 0, "total": 0}
    
    todo_pattern = r'TodoWrite.*?(?:"status"\s*:\s*"in_progress"|"status"\s*:\s*"pending")'
    
    try:
        with open(transcript_path, 'r') as f:
            content = f.read()
            
        pending_todos = len(re.findall(todo_pattern, content, re.IGNORECASE))
        total_todos = len(re.findall(r'TodoWrite', content))
        
        return {
            "incomplete": pending_todos,
            "total": total_todos
        }
    except Exception:
        return {"incomplete": 0, "total": 0}

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
            print(f"Warning: {todo_status['incomplete']} todo items are still incomplete", file=sys.stderr)

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
