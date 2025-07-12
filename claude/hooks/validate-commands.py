#!/usr/bin/env python3
"""
Validate bash commands to prevent use of legacy tools
Blocks: find, grep (without pipes), and other unsafe patterns
"""
import json
import sys
import re
import os

# Define validation rules as a list of (regex pattern, message) tuples
VALIDATION_RULES = [
    (r"\bfind\b(?!.*\|)", "Use 'fd' instead of 'find' for better performance and features"),
    (r"\bgrep\b(?!.*\|)", "Use 'rg' (ripgrep) instead of 'grep' for better performance"),
    (r"\bwhich\b", "Use 'command -v' instead of 'which' for better portability"),
    (r"\bsudo\b", "sudo usage is blocked for security reasons"),
    (r"rm\s+-rf\s+/?", "Dangerous rm -rf command blocked"),
    (r"chmod\s+777", "chmod 777 is insecure - use more restrictive permissions"),
]

def validate_command(command: str) -> list[str]:
    """Validate a bash command and return list of issues found."""
    issues = []
    if not command:
        return issues
    
    for pattern, message in VALIDATION_RULES:
        if re.search(pattern, command, re.IGNORECASE):
            issues.append(message)
    
    return issues

def main():
    """Main entry point for the hook."""
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)
    
    tool_name = input_data.get("tool_name", "")
    tool_input = input_data.get("tool_input", {})
    command = tool_input.get("command", "")
    
    if tool_name != "Bash" or not command:
        sys.exit(0)  # Not a bash command, skip validation
    
    # Validate the command
    issues = validate_command(command)
    
    if issues:
        for message in issues:
            print(f"• {message}", file=sys.stderr)
        # Exit code 2 blocks tool call and shows stderr to Claude
        sys.exit(2)
    
    sys.exit(0)

if __name__ == "__main__":
    main()