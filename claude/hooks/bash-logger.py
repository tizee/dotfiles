#!/usr/bin/env python3
"""
Log Bash command execution to /tmp for debugging truncated output.
"""

import datetime
import json
import sys
from pathlib import Path

LOG_FILE = Path("/tmp/claude-bash.log")
MAX_SIZE_KB = 500
LINES_TO_KEEP = 500


def rotate_log():
    """Rotate log file if it exceeds max size."""
    if not LOG_FILE.exists():
        return
    try:
        if LOG_FILE.stat().st_size > MAX_SIZE_KB * 1024:
            lines = LOG_FILE.read_text().splitlines(keepends=True)
            if len(lines) > LINES_TO_KEEP:
                LOG_FILE.write_text("".join(lines[-LINES_TO_KEEP:]))
    except Exception:
        pass


def main():
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError:
        sys.exit(0)

    if input_data.get("tool_name") != "Bash":
        sys.exit(0)

    tool_input = input_data.get("tool_input", {})
    tool_response = input_data.get("tool_response", {})

    command = tool_input.get("command", "")
    stdout = tool_response.get("stdout", "")
    stderr = tool_response.get("stderr", "")

    rotate_log()

    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    entry = f"""
{'='*60}
[{timestamp}] Command:
{command}
{'─'*60}
stdout:
{stdout}
{'─'*60}
stderr:
{stderr}
{'='*60}
"""

    try:
        with LOG_FILE.open("a") as f:
            f.write(entry)
    except Exception:
        pass

    sys.exit(0)


if __name__ == "__main__":
    main()
