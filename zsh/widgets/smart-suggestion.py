#!/usr/bin/env python3
"""
smart-suggestion.py — Core logic for the ZSH smart-suggestion widget.

Reads a JSON request from stdin, calls `llms` to get a shell command suggestion,
validates the response, and writes a JSON result to stdout.

Input JSON:
  {
    "input": "<current buffer text>",
    "cwd": "/current/dir",
    "shell": "zsh",
    "user": "username",
    "model": "",
    "send_context": true,
    "aliases": "alias output...",
    "history": "recent history...",
    "cwd_dirs": ["dir1", "dir2"],
    "cwd_files": ["file1", "file2"]
  }

Output JSON:
  {"status": "ok", "mode": "=" | "+", "suggestion": "..."}
  {"status": "error", "message": "..."}
"""

import json
import subprocess
import sys


def xml_escape(value: str) -> str:
    return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def build_context_xml(req: dict) -> str:
    parts: list[str] = []
    parts.append("<environment-context>")
    parts.append(f"  <current-directory>{xml_escape(req['cwd'])}</current-directory>")
    parts.append(f"  <shell>{xml_escape(req['shell'])}</shell>")
    parts.append(f"  <user>{xml_escape(req['user'])}</user>")

    parts.append("  <cwd-directories>")
    for d in req.get("cwd_dirs", []):
        parts.append(f"    <dir>{xml_escape(d)}</dir>")
    parts.append("  </cwd-directories>")

    parts.append("  <cwd-files>")
    for f in req.get("cwd_files", []):
        parts.append(f"    <file>{xml_escape(f)}</file>")
    parts.append("  </cwd-files>")

    aliases = req.get("aliases", "")
    if aliases:
        parts.append(f"  <aliases>{xml_escape(aliases)}</aliases>")

    history = req.get("history", "")
    if history:
        parts.append(f"  <recent-history>{xml_escape(history)}</recent-history>")

    parts.append("</environment-context>")
    return "\n".join(parts)


SYSTEM_PROMPT_TEMPLATE = """\
You are Smart Suggestion, an AI shell assistant for zsh.
Return exactly one line and nothing else.
The first character of your response must be = or +.
You will receive environment details inside XML tags.
Treat <current-directory> as the default scope for filesystem operations.
Unless the user explicitly asks otherwise, prefer commands that operate inside <current-directory>.
Prefer relative paths and . over ~ or absolute paths when the task can be solved from <current-directory>.
Do not broaden searches to the home directory, root directory, or the whole filesystem unless the user explicitly asks for that scope.
Use <cwd-directories> and <cwd-files> to infer likely targets in the current directory before searching more broadly.
If the current input is empty, predict the most likely next shell command and prefix it with =.
If the current input is a natural language request, convert it to a shell command and prefix it with =.
If the current input is a partial shell command, return only the missing suffix to append at the cursor and prefix it with +.
For + responses, never repeat the existing input.
Valid examples:
=ffmpeg -i input.mp4 -t 10 -c copy output_10s.mp4
+ -t 10 -c copy output_10s.mp4
Invalid examples:
Here is the command: ffmpeg -i input.mp4 -t 10 -c copy output_10s.mp4
```bash
ffmpeg -i input.mp4 -t 10 -c copy output_10s.mp4
```
Do not use markdown, code fences, comments, or explanations.
The command must be valid for zsh on Unix."""


def build_system_prompt(req: dict) -> str:
    prompt = SYSTEM_PROMPT_TEMPLATE
    if req.get("send_context", True):
        context_xml = build_context_xml(req)
        prompt += "\n\n" + context_xml
    return prompt


def normalize_result(raw: str) -> str | None:
    """Extract the first non-empty line that starts with = or +.
    Returns the cleaned line, or None if no valid line found."""
    cleaned = raw.replace("\r", "")
    for line in cleaned.split("\n"):
        line = line.strip()
        if not line:
            continue
        if line[0] in ("=", "+"):
            return line
    # fallback: return stripped raw if non-empty
    cleaned = cleaned.strip()
    return cleaned if cleaned else None


def validate_result(normalized: str) -> dict:
    """Validate the normalized result string.
    Returns a result dict with status ok/error."""
    if "\n" in normalized:
        return {
            "status": "error",
            "message": "llms returned multiple lines. Expected exactly one line prefixed with = or +.",
        }

    prefix = normalized[0]
    if prefix not in ("=", "+"):
        return {
            "status": "error",
            "message": "llms returned an invalid suggestion prefix. Expected = or +.",
        }

    return {"status": "ok", "mode": prefix, "suggestion": normalized[1:]}


def call_llms(user_input: str, system_prompt: str, model: str) -> dict:
    """Invoke the llms CLI and return a result dict."""
    cmd = ["llms", "prompt"]
    if model:
        cmd += ["-m", model]
    cmd += ["-s", system_prompt, user_input]

    try:
        proc = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
    except FileNotFoundError:
        return {"status": "error", "message": "llms command not found in PATH."}
    except subprocess.TimeoutExpired:
        return {"status": "error", "message": "llms timed out after 30 seconds."}

    if proc.returncode != 0:
        err = proc.stderr.strip() or "llms prompt failed."
        return {"status": "error", "message": err}

    normalized = normalize_result(proc.stdout)
    if normalized is None:
        return {"status": "error", "message": "llms returned an empty suggestion."}

    return validate_result(normalized)


def main() -> None:
    raw = sys.stdin.read()
    try:
        req = json.loads(raw)
    except json.JSONDecodeError as e:
        json.dump({"status": "error", "message": f"Invalid JSON input: {e}"}, sys.stdout)
        sys.exit(1)

    system_prompt = build_system_prompt(req)
    result = call_llms(
        user_input=req.get("input", ""),
        system_prompt=system_prompt,
        model=req.get("model", ""),
    )
    json.dump(result, sys.stdout)


if __name__ == "__main__":
    main()
