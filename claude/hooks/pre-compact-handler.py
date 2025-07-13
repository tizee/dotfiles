#!/usr/bin/env python3
"""
PreCompact Hook Handler for Claude Code

This script handles the PreCompact event, which runs before Claude Code 
is about to run a compact operation. It provides logging and monitoring
for conversation compaction activities.
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


def log_pre_compact_event(data):
    """Log pre-compact event to a file for debugging purposes."""
    log_file = os.path.expanduser("~/.claude/logs/pre-compact-events.log")
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    
    # Rotate log file if it's too large
    rotate_log_file(log_file)
    
    with open(log_file, "a") as f:
        timestamp = datetime.now().isoformat()
        f.write(f"[{timestamp}] PreCompact Event:\n")
        f.write(json.dumps(data, indent=2))
        f.write("\n" + "="*50 + "\n")


def main():
    """Main handler for pre-compact events."""
    try:
        # Read JSON input from stdin
        input_data = json.load(sys.stdin)
        
        # Log the event
        log_pre_compact_event(input_data)
        
        # Extract useful information
        trigger = input_data.get("trigger", "unknown")
        custom_instructions = input_data.get("custom_instructions", "")
        
        # You could add custom logic here based on trigger type
        # For example, different behavior for manual vs auto compaction
        
        # Exit with code 0 to allow normal compaction
        sys.exit(0)
        
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error processing pre-compact event: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()