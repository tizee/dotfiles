#!/usr/bin/env python3
"""
SubagentStop Hook Handler for Claude Code

This script handles the SubagentStop event, which runs when a Claude Code 
subagent (Task tool call) has finished responding. It provides logging and 
can optionally block stopping if certain conditions are met.
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


def log_subagent_stop_event(data):
    """Log subagent stop event to a file for debugging purposes."""
    log_file = os.path.expanduser("~/.claude/logs/subagent-stop-events.log")
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    
    # Rotate log file if it's too large
    rotate_log_file(log_file)
    
    with open(log_file, "a") as f:
        timestamp = datetime.now().isoformat()
        f.write(f"[{timestamp}] SubagentStop Event:\n")
        f.write(json.dumps(data, indent=2))
        f.write("\n" + "="*50 + "\n")


def generate_session_summary(transcript_path):
    """Generate simple session summary for current interaction."""
    if not transcript_path or not os.path.exists(transcript_path):
        return {}
    
    summary = {
        'total_tools': 0,
        'bash_commands': 0,
        'file_operations': 0,
        'success_operations': 0,
        'error_operations': 0,
        'session_length': 0
    }
    
    try:
        with open(transcript_path, 'r') as f:
            content = f.read()
            lines = content.split('\n')
            summary['session_length'] = len(lines)
            
            # Simple tool counts
            tool_matches = re.findall(r'"tool_name"\s*:\s*"([^"]+)"', content)
            summary['total_tools'] = len(tool_matches)
            
            # Count specific tool types
            summary['bash_commands'] = tool_matches.count('Bash')
            summary['file_operations'] = len([t for t in tool_matches if t in ['Read', 'Write', 'Edit', 'MultiEdit']])
            
            # Success/error counts
            summary['success_operations'] = len(re.findall(r'"success":\s*true', content))
            summary['error_operations'] = len(re.findall(r'"success":\s*false', content))
            
    except Exception:
        pass
    
    return summary


def log_session_summary_simple(summary):
    """Log simple session summary."""
    log_file = os.path.expanduser("~/.claude/logs/session-summary-simple.log")
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    
    with open(log_file, "a") as f:
        timestamp = datetime.now().isoformat()
        f.write(f"[{timestamp}] Session Summary:\n")
        f.write(f"  Total Tools Used: {summary.get('total_tools', 0)}\n")
        f.write(f"  Bash Commands: {summary.get('bash_commands', 0)}\n")
        f.write(f"  File Operations: {summary.get('file_operations', 0)}\n")
        f.write(f"  Success Operations: {summary.get('success_operations', 0)}\n")
        f.write(f"  Error Operations: {summary.get('error_operations', 0)}\n")
        f.write(f"  Session Length: {summary.get('session_length', 0)} lines\n")
        f.write("-" * 40 + "\n")


def store_performance_json(data):
    """Store lightweight performance metrics in JSON format."""
    json_file = os.path.expanduser("~/.claude/logs/subagent-metrics.json")
    os.makedirs(os.path.dirname(json_file), exist_ok=True)
    
    try:
        # Load existing metrics
        metrics = []
        if os.path.exists(json_file):
            try:
                with open(json_file, 'r') as f:
                    metrics = json.load(f)
                    # Ensure it's a list
                    if not isinstance(metrics, list):
                        metrics = []
            except (json.JSONDecodeError, IOError):
                metrics = []
        
        # Parse transcript for basic metrics
        transcript_path = data.get("transcript_path", "")
        stats = {
            'transcript_lines': 0,
            'total_tools': 0,
            'success_count': 0,
            'error_count': 0,
            'bash_commands': 0,
            'file_operations': 0
        }
        
        if transcript_path and os.path.exists(transcript_path):
            with open(transcript_path, 'r') as f:
                content = f.read()
                lines = content.split('\n')
                stats['transcript_lines'] = len(lines)
                stats['total_tools'] = len(re.findall(r'"tool_name"\s*:\s*"[A-Za-z]+"', content))
                stats['success_count'] = len(re.findall(r'"success"\s*:\s*true', content))
                stats['error_count'] = len(re.findall(r'"success"\s*:\s*false', content))
                stats['bash_commands'] = len(re.findall(r'"tool_name"\s*:\s*"Bash"', content))
                stats['file_operations'] = len(re.findall(r'"tool_name"\s*:\s*"(Read|Write|Edit|MultiEdit)"', content))
        
        # Add new metric
        metric = {
            'timestamp': datetime.now().isoformat(),
            'session_id': data.get('session_id', 'unknown'),
            'stats': stats
        }
        
        metrics.append(metric)
        
        # Keep only last 100 metrics to avoid bloat
        metrics = metrics[-100:]
        
        with open(json_file, 'w') as f:
            json.dump(metrics, f, indent=2)
        
    except Exception as e:
        print(f"Warning: Could not store performance JSON: {e}", file=sys.stderr)


def main():
    """Main handler for subagent stop events."""
    try:
        # Read JSON input from stdin
        input_data = json.load(sys.stdin)
        
        # Log the event
        log_subagent_stop_event(input_data)
        
        # Check if stop hook is already active
        if input_data.get("stop_hook_active", False):
            # Prevent infinite loops
            print("Subagent stop hook already active, allowing normal stop", file=sys.stderr)
            sys.exit(0)
        
        # Generate session summary for current interaction
        session_summary = generate_session_summary(input_data.get("transcript_path", ""))
        log_session_summary_simple(session_summary)
        
        # Store lightweight performance metrics in JSON
        store_performance_json(input_data)
        
        # Exit with code 0 to allow normal stopping
        sys.exit(0)
        
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error processing subagent stop event: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()