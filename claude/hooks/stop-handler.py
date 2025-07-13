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
        
        # You can add custom logic here to decide whether to block stopping
        # For now, we'll allow normal stopping
        
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