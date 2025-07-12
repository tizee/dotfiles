#!/usr/bin/env python3
"""
Validate file paths for security and safety
Blocks: path traversal, sensitive files, and git directory modifications
"""
import json
import sys
import os

# Define validation rules for sensitive files and directories
SENSITIVE_PATTERNS = [
    (r"\.\./", "Path traversal detected - use absolute paths"),
    (r"\.env$", "Environment file blocked - contains sensitive keys/secrets"),
    (r"\.env\.", "Environment file blocked - contains sensitive configuration"),
    (r"\.env-", "Environment file blocked - contains sensitive configuration"),
    (r"secrets\.json$", "Secrets file blocked - contains sensitive data"),
    (r"\.git/", "Git directory access blocked"),
    (r"\.ssh/", "SSH directory access blocked"),
    (r"id_rsa", "Private key access blocked"),
    (r"\.pem$", "Certificate/key file access blocked"),
    (r"\.key$", "Key file access blocked"),
    (r"\.crt$", "Certificate file access blocked"),
    (r"\.aws/", "AWS credentials access blocked"),
    (r"\.kube/", "Kubernetes config access blocked"),
    (r"\.docker/", "Docker config access blocked"),
    (r"/etc/", "System configuration access blocked"),
    (r"/usr/", "System directory access blocked"),
    (r"/var/", "System directory access blocked"),
    (r"/proc/", "System process directory access blocked"),
    (r"/sys/", "System directory access blocked"),
]

def validate_file_path(file_path: str) -> list[str]:
    """Validate a file path and return list of issues found."""
    issues = []
    if not file_path:
        return issues

    # Check for dangerous patterns
    import re
    for pattern, message in SENSITIVE_PATTERNS:
        if re.search(pattern, file_path):
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
    if tool_name not in ["Write", "Edit", "MultiEdit", "Read"]:
        sys.exit(0)  # Not a file operation tool

    tool_input = input_data.get("tool_input", {})
    file_path = tool_input.get("file_path", "")

    # Validate the file path
    issues = validate_file_path(file_path)

    if issues:
        for message in issues:
            print(f"• {message}", file=sys.stderr)
        # Exit code 2 blocks tool call and shows stderr to Claude
        sys.exit(2)

    sys.exit(0)

if __name__ == "__main__":
    main()
