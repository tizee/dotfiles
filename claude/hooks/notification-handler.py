#!/usr/bin/env python3
"""
Custom notification handler for Claude Code
Provides visual feedback for important events
"""

import json
import locale
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


def get_system_language():
    """Detect system language and return True if Chinese, False for English."""
    try:
        # Try multiple methods to detect Chinese locale
        lang = os.environ.get("LANG", "")
        if "zh" in lang.lower():
            return True

        # Check system locale (using modern approach)
        try:
            system_locale = locale.getlocale()[0]
            if system_locale and "zh" in system_locale.lower():
                return True
        except Exception:
            # Silently ignore locale detection errors - intentional
            pass

        # Check macOS system language
        try:
            result = subprocess.run(
                ["defaults", "read", "-g", "AppleLanguages"],
                capture_output=True,
                text=True,
                timeout=2,
                check=False,
            )
            if result.returncode == 0 and "zh" in result.stdout.lower():
                return True
        except Exception:
            # Silently ignore subprocess errors - intentional
            pass

        return False
    except Exception:
        # Fallback to English if any unexpected error occurs
        return False


def get_localized_strings(is_chinese=False):
    """Get localized strings based on language."""
    if is_chinese:
        return {
            "permission": "Claude Code - 权限请求 🔐",
            "waiting": "Claude Code - 等待中 ⏳",
            "completed": "Claude Code - 完成 ✅",
            "error": "Claude Code - 错误 ❌",
            "blocked": "Claude Code - 操作被阻止 🚫",
            "notification": "Claude Code - 通知 📢",
        }
    return {
        "permission": "Claude Code - Permission Required 🔐",
        "waiting": "Claude Code - Waiting ⏳",
        "completed": "Claude Code - Completed ✅",
        "error": "Claude Code - Error ❌",
        "blocked": "Claude Code - Blocked 🚫",
        "notification": "Claude Code - Notification 📢",
    }


def send_notification(title: str, message: str, sound_name: str | None = None):
    """Send immediate notification using terminal-notifier with custom sound."""
    try:
        # Use terminal-notifier for reliable notifications
        cmd = ["terminal-notifier", "-title", title, "-message", message]
        if sound_name:
            cmd.extend(["-sound", sound_name])

        subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

        # Also log to stderr for debugging
        sound_info = f" (🔊 {sound_name})" if sound_name else ""
        print(f"🔔 {title}: {message}{sound_info}", file=sys.stderr)

    except Exception as e:
        # Fallback: terminal bell and message
        print(f"\a🔔 {title}: {message} (Error: {e})", file=sys.stderr)


def process_notification(data: dict):
    """Process incoming notification data with different sounds and localized titles."""
    message = data.get("message", "")
    # Detect system language and get localized strings
    is_chinese = get_system_language()
    strings = get_localized_strings(is_chinese)

    # Determine notification type and sound based on message content
    if "permission" in message.lower():
        title = strings["permission"]
        send_notification(title, message, sound_name="Ping")  # 清脆提示音 - 需要注意
    elif "waiting" in message.lower() or "idle" in message.lower():
        title = strings["waiting"]
        send_notification(title, message, sound_name="Tink")  # 轻柔提示音 - 等待状态
    elif "completed" in message.lower() or "success" in message.lower():
        title = strings["completed"]
        send_notification(title, message, sound_name="default")  # 系统默认声音
    elif "error" in message.lower() or "failed" in message.lower():
        title = strings["error"]
        send_notification(title, message, sound_name="Basso")  # 错误音效
    elif "blocked" in message.lower() or "denied" in message.lower():
        title = strings["blocked"]
        send_notification(title, message, sound_name="Funk")  # 阻止音效
    else:
        title = strings["notification"]
        send_notification(title, message, sound_name="Glass")  # 默认通知音效


def main():
    """Main entry point for the notification hook."""
    # Debug: Always log that hook was called
    debug_log = Path.home() / ".claude" / "logs" / "notification-hook-debug.log"
    debug_log.parent.mkdir(parents=True, exist_ok=True)

    with debug_log.open("a") as f:
        timestamp = datetime.now(timezone.utc).isoformat()
        f.write(f"Notification hook called at {timestamp}\n")

    try:
        input_data = json.load(sys.stdin)
        # Debug: Log input data
        with debug_log.open("a") as f:
            f.write(f"Input data: {json.dumps(input_data, indent=2)}\n")
    except json.JSONDecodeError as e:
        # Debug: Log JSON decode errors
        with debug_log.open("a") as f:
            f.write(f"JSON decode error: {e}\n")
        sys.exit(0)

    process_notification(input_data)
    sys.exit(0)


if __name__ == "__main__":
    main()
