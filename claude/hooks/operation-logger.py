#!/usr/bin/env python3
"""
Log file operations with automatic log rotation
Maintains a rolling log file with size-based rotation
"""
import json
import sys
import os
import datetime
from pathlib import Path

def rotate_log(log_file: Path, max_size_kb: int = 50, lines_to_keep: int = 100):
    """Rotate log file if it exceeds max size."""
    if not log_file.exists():
        return
    
    try:
        file_size = log_file.stat().st_size
        max_size_bytes = max_size_kb * 1024
        
        if file_size > max_size_bytes:
            # Read all lines
            with open(log_file, 'r') as f:
                lines = f.readlines()
            
            # Keep only the last N lines
            if len(lines) > lines_to_keep:
                lines_to_write = lines[-lines_to_keep:]
            else:
                lines_to_write = lines
            
            # Write back the kept lines
            with open(log_file, 'w') as f:
                f.writelines(lines_to_write)
                
    except Exception as e:
        # Silently fail if we can't rotate the log
        pass

def log_operation(tool_name: str, file_path: str, success: bool):
    """Log a file operation with rotation."""
    log_dir = Path.home() / '.claude' / 'logs'
    log_dir.mkdir(parents=True, exist_ok=True)
    
    log_file = log_dir / 'operations.log'
    
    # Rotate log if needed
    rotate_log(log_file)
    
    # Create log entry
    timestamp = datetime.datetime.now().isoformat()
    status = "SUCCESS" if success else "FAILED"
    log_entry = f"{timestamp} - {tool_name} - {file_path} - {status}\n"
    
    try:
        with open(log_file, 'a') as f:
            f.write(log_entry)
    except Exception:
        # Silently fail if we can't write to log
        pass

def main():
    """Main entry point for the hook."""
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError:
        sys.exit(0)  # Silently skip on JSON errors
    
    tool_name = input_data.get("tool_name", "")
    if tool_name not in ["Write", "Edit", "MultiEdit"]:
        sys.exit(0)  # Only log file operations
    
    tool_input = input_data.get("tool_input", {})
    file_path = tool_input.get("file_path", "")
    
    tool_response = input_data.get("tool_response", {})
    success = tool_response.get("success", False)
    
    log_operation(tool_name, file_path, success)
    sys.exit(0)

if __name__ == "__main__":
    main()