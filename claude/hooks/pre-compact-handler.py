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


def analyze_compaction_impact(transcript_path):
    """Analyze conversation to determine compaction impact."""
    if not transcript_path or not os.path.exists(transcript_path):
        return {'file_size': 0, 'line_count': 0, 'estimated_reduction': 0}
    
    analysis = {
        'file_size': 0,
        'line_count': 0,
        'estimated_reduction': 0,
        'tool_usage_density': 0,
        'conversations_combined': 0,
        'key_insights': []
    }
    
    try:
        # Get basic file metrics
        analysis['file_size'] = os.path.getsize(transcript_path)
        
        with open(transcript_path, 'r') as f:
            lines = f.readlines()
            analysis['line_count'] = len(lines)
        
        # Estimate reduction based on content patterns
        redundant_pattern = re.compile(
            r'(content|content[^"]*):\s*"Creating\s+todo|"Content[^"]*:\s*"Starting\s+task'
        )
        redundant_count = len(re.findall(redundant_pattern, '\n'.join(lines)))
        analysis['estimated_reduction'] = max(0, redundant_count * 50)  # Rough estimate
        
        # Count tool density
        tool_usage = len(re.findall(r'"tool_name"', '\n'.join(lines)))
        if analysis['line_count'] > 0:
            analysis['tool_usage_density'] = tool_usage / analysis['line_count']
        
        # Identify key insights to preserve
        insight_keywords = [
            'error', 'warning', 'success', 'completed',
            'failed', 'critical', 'important', 'optimization'
        ]
        
        for keyword in insight_keywords:
            if any(keyword in line.lower() for line in lines[::-1][:100]):  # Last 100 lines
                analysis['key_insights'].append(keyword)
        
    except Exception as e:
        print(f"Warning: Could not analyze compaction impact: {e}", file=sys.stderr)
    
    return analysis


def log_compaction_tracking(trigger, custom_instructions, analysis):
    """Log compaction tracking data."""
    log_file = os.path.expanduser("~/.claude/logs/compaction-tracking.log")
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    
    with open(log_file, "a") as f:
        timestamp = datetime.now().isoformat()
        f.write(f"[{timestamp}] Compaction Event:\n")
        f.write(f"  Trigger: {trigger}\n")
        f.write(f"  Custom Instructions: {custom_instructions[:50]}...\n" if custom_instructions else "  Custom Instructions: None\n")
        f.write(f"  Original Size: {analysis.get('line_count', 0)} lines ({analysis.get('file_size', 0)} bytes)\n")
        f.write(f"  Estimated Reduction: {analysis.get('estimated_reduction', 0)} bytes\n")
        f.write(f"  Tool Density: {analysis.get('tool_usage_density', 0):.3f}\n")
        f.write(f"  Key Insights: {', '.join(analysis.get('key_insights', []))}\n")
        f.write("-" * 60 + "\n")


def store_cleanup_recommendations(analysis):
    """Store cleanup recommendations for future reference."""
    cleanup_file = os.path.expanduser("~/.claude/logs/cleanup-recommendations.json")
    os.makedirs(os.path.dirname(cleanup_file), exist_ok=True)
    
    try:
        recommendations = []
        
        if os.path.exists(cleanup_file):
            with open(cleanup_file, 'r') as f:
                try:
                    recommendations = json.load(f)
                except json.JSONDecodeError:
                    recommendations = []
        
        # Add new recommendation
        recommendation = {
            'timestamp': datetime.now().isoformat(),
            'trigger_type': analysis.get('trigger', 'auto'),
            'insights_to_preserve': analysis.get('key_insights', []),
            'file_metrics': {
                'lines': analysis.get('line_count', 0),
                'bytes': analysis.get('file_size', 0),
                'estimated_reduction': analysis.get('estimated_reduction', 0)
            }
        }
        
        recommendations.append(recommendation)
        
        # Keep only last 100 recommendations
        recommendations = recommendations[-100:]
        
        with open(cleanup_file, 'w') as f:
            json.dump(recommendations, f, indent=2)
            
    except Exception as e:
        print(f"Warning: Could not store cleanup recommendations: {e}", file=sys.stderr)


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
        
        # Analyze conversation size and impact
        compaction_analysis = analyze_compaction_impact(input_data.get("transcript_path", ""))
        
        # Log compaction details
        log_compaction_tracking(trigger, custom_instructions, compaction_analysis)
        
        # Store cleanup recommendations
        store_cleanup_recommendations(compaction_analysis)
        
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