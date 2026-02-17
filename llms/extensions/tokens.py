"""Extension slash command to estimate token counts for a file.

Usage:
    /tokens <file_path>

Examples:
    /tokens README.md
    /tokens /absolute/path/to/file.py
    /tokens ./relative/path/to/file.ts

Place in: ~/.config/llms/extensions/tokens.py
Verify:  /extensions   or   /reload
"""

from __future__ import annotations

import os
from pathlib import Path

from llms.agent.extensions import ExtensionAPI
from llms.lib.token_tracker import count_tokens, format_token_count


def setup(api: ExtensionAPI):
    api.register_command(
        name="tokens",
        description="Estimate token count for a file",
        handler=_handle_tokens,
        argument_hint="<file_path>",
    )


def _handle_tokens(ctx):
    """Estimate token count for a given file path.

    Supports both relative and absolute paths. Expands ~ for home directory.
    Displays: character count, line count, and estimated token count.
    """
    path_str = ctx.args.strip()
    if not path_str:
        ctx.send_result("Usage: /tokens <file_path>")
        return

    # Resolve path: expand ~ and make absolute
    path = Path(path_str).expanduser()

    # If relative, resolve from current working directory
    if not path.is_absolute():
        # Get current working directory from environment
        cwd = os.getcwd()
        path = Path(cwd) / path

    if not path.exists():
        ctx.send_result(f"File not found: {path}")
        return

    if not path.is_file():
        ctx.send_result(f"Not a file: {path}")
        return

    try:
        content = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        ctx.send_result(f"Cannot read file (not UTF-8): {path}")
        return
    except Exception as e:
        ctx.send_result(f"Error reading file: {e}")
        return

    # Calculate metrics
    char_count = len(content)
    line_count = content.count("\n") + (1 if content and not content.endswith("\n") else 0)
    token_count = count_tokens(content)

    # Format output
    result = [
        f"File: {path}",
        f"Characters: {char_count:,}",
        f"Lines: {line_count:,}",
        f"Tokens: ~{format_token_count(token_count)} ({token_count:,})",
    ]

    ctx.send_result("\n".join(result))
