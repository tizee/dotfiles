#!/usr/bin/env python3
"""
Custom notification handler for Claude Code
Provides visual feedback for important events

Sound Configuration:
  - Supports both system sounds (via terminal-notifier) and custom audio files (via afplay)
  - Use SoundConfig to customize sounds through dependency injection
"""

import json
import locale
import os
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path


@dataclass
class SoundConfig:
    """
    Configuration for notification sounds.
    Sound names correspond to files in ~/Library/Sounds/ or /System/Library/Sounds/.

    Usage:
        # Use default config with custom sounds
        config = get_default_sound_config()

        # Override specific sounds
        config = SoundConfig(
            permission="yes-me-lord",  # Custom sound in ~/Library/Sounds/
            idle="Tink",               # System sound
            default="Glass"
        )

    To add custom sounds:
        1. Convert to AIFF: afconvert input.mp3 ~/Library/Sounds/name.aiff -f AIFF -d BEI16
        2. Use the name (without extension) in SoundConfig
    """

    permission: str = "Ping"  # Clear alert - needs attention
    idle: str = "Tink"  # Soft notification - waiting state
    completed: str = "default"  # System default
    error: str = "Basso"  # Error sound
    blocked: str = "Funk"  # Blocked action
    default: str = "Glass"  # Default notification


def get_default_sound_config() -> SoundConfig:
    """Get default sound configuration with custom sounds from ~/Library/Sounds/."""
    return SoundConfig(
        permission="yes-me-lord",  # Custom: ~/Library/Sounds/yes-me-lord.aiff
        idle="ready-to-work",  # Custom: ~/Library/Sounds/ready-to-work.aiff
        completed="work-complete",  # Custom: ~/Library/Sounds/work-complete.aiff
        error="huh",  # Custom: ~/Library/Sounds/huh.aiff
        blocked="Funk",
        default="more-work",  # Custom: ~/Library/Sounds/more-work.aiff
    )


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


def play_sound(sound: str | None) -> None:
    """
    Play a sound using afplay (bypasses terminal-notifier sound issues).

    Args:
        sound: Sound name (searches ~/Library/Sounds/ then /System/Library/Sounds/)
    """
    if not sound:
        return

    # Search paths for sound files
    search_paths = [
        Path.home() / "Library" / "Sounds" / f"{sound}.aiff",
        Path(f"/System/Library/Sounds/{sound}.aiff"),
    ]

    for sound_path in search_paths:
        if sound_path.exists():
            subprocess.Popen(
                ["afplay", str(sound_path)],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            return


def send_notification(title: str, message: str, sound: str | None = None) -> None:
    """
    Send notification using terminal-notifier + afplay for sound.

    Args:
        title: Notification title
        message: Notification message
        sound: Sound name (from ~/Library/Sounds/ or /System/Library/Sounds/)
    """
    try:
        # Visual notification via terminal-notifier (without sound)
        cmd = ["terminal-notifier", "-title", title, "-message", message]
        subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

        # Play sound via afplay (more reliable)
        play_sound(sound)

        # Log to stderr for debugging
        sound_info = f" (🔊 {sound})" if sound else ""
        print(f"🔔 {title}: {message}{sound_info}", file=sys.stderr)

    except Exception as e:
        # Fallback: terminal bell and message
        print(f"\a🔔 {title}: {message} (Error: {e})", file=sys.stderr)


def process_notification(
    data: dict,
    sound_config: SoundConfig | None = None,
) -> None:
    """
    Process incoming notification data with different sounds and localized titles.

    Args:
        data: Notification data dict with 'message' key
        sound_config: Optional custom sound configuration (dependency injection)
    """
    message = data.get("message", "")

    # Use injected config or get default
    config = sound_config or get_default_sound_config()

    # Detect system language and get localized strings
    is_chinese = get_system_language()
    strings = get_localized_strings(is_chinese)

    # Determine notification type and sound based on message content
    if "permission" in message.lower():
        title = strings["permission"]
        send_notification(title, message, sound=config.permission)
    elif "waiting" in message.lower() or "idle" in message.lower():
        title = strings["waiting"]
        send_notification(title, message, sound=config.idle)
    elif "completed" in message.lower() or "success" in message.lower():
        title = strings["completed"]
        send_notification(title, message, sound=config.completed)
    elif "error" in message.lower() or "failed" in message.lower():
        title = strings["error"]
        send_notification(title, message, sound=config.error)
    elif "blocked" in message.lower() or "denied" in message.lower():
        title = strings["blocked"]
        send_notification(title, message, sound=config.blocked)
    else:
        title = strings["notification"]
        send_notification(title, message, sound=config.default)


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
