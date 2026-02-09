#!/usr/bin/env python3
"""
Claude Code Quota Checker (Web API via cURL)

Uses curl command to fetch quota, which can bypass some Cloudflare detection.

Usage:
    # Automatically get cookies from Firefox and use curl
    python3 claude_quota_curl.py

    # Specify Firefox profile
    python3 claude_quota_curl.py --profile xxx.default

    # Manually provide cookie header
    python3 claude_quota_curl.py --cookie "sessionKey=sk-ant-...; cf_clearance=..."

    # Show raw API response
    python3 claude_quota_curl.py --raw
"""

import argparse
import base64
import json
import os
import platform
import re
import sqlite3
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional


CLAUDE_BASE_URL = "https://claude.ai/api"

# Debounce configuration
DEBOUNCE_CACHE_DIR = Path("/tmp/claude_quota_cache")
DEBOUNCE_DEFAULT_INTERVAL = 60  # Default 60 seconds


class DebounceManager:
    """
    Manages debounce mechanism for API calls

    Prevents frequent API requests in high-frequency call scenarios such as status bars
    """

    def __init__(self, interval_seconds: int = DEBOUNCE_DEFAULT_INTERVAL):
        self.interval = interval_seconds
        self.cache_dir = DEBOUNCE_CACHE_DIR
        self.last_call_file = self.cache_dir / "last_call_time"
        self.cached_result_file = self.cache_dir / "cached_result.json"
        self.refresh_lock_file = self.cache_dir / "refresh.lock"

    def _ensure_cache_dir(self):
        """Ensure cache directory exists"""
        self.cache_dir.mkdir(parents=True, exist_ok=True)

    def get_last_call_time(self) -> Optional[float]:
        """Get last API call time"""
        try:
            if self.last_call_file.exists():
                return float(self.last_call_file.read_text().strip())
        except (ValueError, IOError):
            pass
        return None

    def get_cached_result(self) -> Optional[dict]:
        """Get cached result"""
        try:
            if self.cached_result_file.exists():
                data = json.loads(self.cached_result_file.read_text())
                # Check if cache has expired (2x interval time)
                cache_time = data.get("_cache_time", 0)
                if time.time() - cache_time < self.interval * 2:
                    return data
        except (json.JSONDecodeError, IOError, KeyError):
            pass
        return None

    def should_skip_call(self) -> tuple[bool, Optional[dict], float, bool]:
        """
        Check whether the API call should be skipped

        Returns:
            (should_skip, cached_data, remaining_seconds, needs_refresh)
            - should_skip=True, needs_refresh=False: within interval, use cache
            - should_skip=True, needs_refresh=True: interval expired, stale cache returned, refresh in background
            - should_skip=False: no cache available, must call API synchronously
        """
        last_call = self.get_last_call_time()
        now = time.time()
        cached = self.get_cached_result()

        if last_call is None:
            # No previous call recorded; if stale cache somehow exists, still do a fresh call
            return False, None, 0.0, False

        elapsed = now - last_call
        if elapsed < self.interval:
            # Within debounce interval, use cache
            remaining = self.interval - elapsed
            return True, cached, remaining, False

        # Interval expired - need a refresh
        if cached:
            # Return stale cache immediately, signal that background refresh is needed
            return True, cached, 0.0, True

        # No cache at all, must call API synchronously
        return False, None, 0.0, False

    def record_call(self, result: Optional[dict] = None):
        """Record API call time and result"""
        self._ensure_cache_dir()
        now = time.time()

        # Record call time
        self.last_call_file.write_text(str(now))

        # Cache result
        if result is not None:
            result_to_cache = dict(result)
            result_to_cache["_cache_time"] = now
            self.cached_result_file.write_text(
                json.dumps(result_to_cache, indent=2, ensure_ascii=False)
            )

    def try_acquire_refresh_lock(self, timeout: int = 60) -> bool:
        """
        Try to acquire the background refresh lock.

        Uses a lock file with PID. Stale locks (process dead or older than
        timeout seconds) are automatically cleaned up.

        Returns True if lock was acquired.
        """
        self._ensure_cache_dir()
        try:
            if self.refresh_lock_file.exists():
                content = self.refresh_lock_file.read_text().strip()
                parts = content.split(":", 1)
                if len(parts) == 2:
                    pid, lock_time = int(parts[0]), float(parts[1])
                    # Check if the locking process is still alive
                    try:
                        os.kill(pid, 0)
                        # Process alive - check if lock is stale by timeout
                        if time.time() - lock_time < timeout:
                            return False
                        # Lock timed out, fall through to acquire
                    except OSError:
                        # Process dead, stale lock - fall through to acquire
                        pass

            # Write our PID and timestamp
            self.refresh_lock_file.write_text(f"{os.getpid()}:{time.time()}")
            return True
        except IOError:
            return False

    def release_refresh_lock(self):
        """Release the background refresh lock"""
        try:
            if self.refresh_lock_file.exists():
                self.refresh_lock_file.unlink()
        except IOError:
            pass

    def clear_cache(self):
        """Clear cache"""
        try:
            if self.last_call_file.exists():
                self.last_call_file.unlink()
            if self.cached_result_file.exists():
                self.cached_result_file.unlink()
            if self.refresh_lock_file.exists():
                self.refresh_lock_file.unlink()
        except IOError:
            pass


class ClaudeAuthError(Exception):
    """Authentication related error"""
    pass


class ClaudeAPIError(Exception):
    """API call error"""
    pass


@dataclass
class CookieInfo:
    """Cookie information"""
    header: str
    source: str


def get_firefox_profiles_dir() -> Path:
    """Get Firefox profiles directory"""
    system = platform.system()
    home = Path.home()

    if system == "Darwin":
        return home / "Library/Application Support/Firefox/Profiles"
    elif system == "Linux":
        return home / ".mozilla/firefox"
    elif system == "Windows":
        appdata = os.environ.get("APPDATA")
        if appdata:
            return Path(appdata) / "Mozilla/Firefox/Profiles"
        return home / "AppData/Roaming/Mozilla/Firefox/Profiles"
    else:
        raise ClaudeAuthError(f"Unsupported operating system: {system}")


def find_firefox_profiles() -> list[Path]:
    """Find all Firefox profiles"""
    profiles_dir = get_firefox_profiles_dir()

    if not profiles_dir.exists():
        raise ClaudeAuthError(f"Firefox profiles directory does not exist: {profiles_dir}")

    profiles = []
    for item in profiles_dir.iterdir():
        if item.is_dir():
            profiles.append(item)

    profiles.sort(key=lambda p: p.stat().st_mtime, reverse=True)
    return profiles


def extract_all_claude_cookies(cookies_db: Path) -> dict[str, str]:
    """Extract all claude.ai related cookies from Firefox cookies.sqlite"""
    cookies = {}

    if not cookies_db.exists():
        return cookies

    try:
        import tempfile
        import shutil

        with tempfile.NamedTemporaryFile(suffix=".sqlite", delete=False) as tmp:
            tmp_path = Path(tmp.name)

        try:
            shutil.copy2(cookies_db, tmp_path)

            conn = sqlite3.connect(tmp_path)
            cursor = conn.cursor()

            cursor.execute("""
                SELECT name, value, host
                FROM moz_cookies
                WHERE host = 'claude.ai' OR host LIKE '%.claude.ai'
                ORDER BY expiry DESC
            """)

            rows = cursor.fetchall()
            conn.close()

            for name, value, host in rows:
                if name not in cookies:
                    cookies[name] = value

        finally:
            try:
                tmp_path.unlink()
            except:
                pass

    except Exception as e:
        raise ClaudeAuthError(f"Failed to read Firefox cookies: {e}")

    return cookies


def build_cookie_header(cookies_db: Path) -> str:
    """Build Cookie header"""
    cookies = extract_all_claude_cookies(cookies_db)

    if not cookies:
        return ""

    cookie_parts = [f"{name}={value}" for name, value in cookies.items()]
    return "; ".join(cookie_parts)


def get_cookies_from_firefox(profile: Optional[str] = None) -> CookieInfo:
    """Get cookies from Firefox"""
    if profile:
        profiles_dir = get_firefox_profiles_dir()
        profile_path = profiles_dir / profile
        if not profile_path.exists():
            raise ClaudeAuthError(f"Specified profile does not exist: {profile_path}")
        profiles = [profile_path]
    else:
        profiles = find_firefox_profiles()

    if not profiles:
        raise ClaudeAuthError("No Firefox profiles found")

    for prof in profiles:
        cookies_db = prof / "cookies.sqlite"
        cookie_header = build_cookie_header(cookies_db)

        if "sessionKey" in cookie_header:
            return CookieInfo(
                header=cookie_header,
                source=f"Firefox profile: {prof.name}"
            )

    raise ClaudeAuthError("Claude sessionKey not found in Firefox")


def curl_request(url: str, cookie_header: str, timeout: int = 30) -> dict:
    """
    Send request using curl

    curl can better simulate browser behavior and bypass some Cloudflare detection
    """
    cmd = [
        "curl",
        "-s",  # Silent mode
        "-L",  # Follow redirects
        "--http1.1",  # Use HTTP/1.1, sometimes less likely to be blocked than HTTP/2
        "-H", f"Cookie: {cookie_header}",
        "-H", "Accept: application/json",
        "-H", "Accept-Language: en-US,en;q=0.9",
        "-H", "Referer: https://claude.ai/",
        "-H", "Origin: https://claude.ai",
        "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
        "--compressed",
        "--max-time", str(timeout),
        url,
    ]

    try:
        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise ClaudeAPIError(f"curl request failed: {result.stderr}")

        # Check if the response is HTML
        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            # Extract title
            title_match = re.search(r'<title>(.*?)</title>', text, re.IGNORECASE)
            title = title_match.group(1) if title_match else "Unknown"

            if "Just a moment" in text or "challenge" in text.lower():
                raise ClaudeAPIError(
                    f"Encountered Cloudflare challenge page: {title}\n"
                    f"\nSolution:\n"
                    f"1. Visit https://claude.ai in Firefox and complete the challenge\n"
                    f"2. Then re-run this script\n"
                    f"3. Or try using --impersonate mode (requires curl-impersonate)"
                )
            elif "login" in text.lower() or "sign in" in text.lower():
                raise ClaudeAPIError(
                    f"Login required: {title}\n"
                    f"Please log in to claude.ai in Firefox"
                )
            else:
                raise ClaudeAPIError(
                    f"API returned an HTML page: {title}\n"
                    f"First 500 chars of response: {text[:500]}"
                )

        return json.loads(text)

    except json.JSONDecodeError as e:
        raise ClaudeAPIError(f"Failed to parse JSON response: {e}\nResponse: {result.stdout[:500]}")
    except FileNotFoundError:
        raise ClaudeAPIError("curl command not found, please install curl")


def fetch_organizations(cookie_header: str) -> dict:
    """Fetch organization list"""
    url = f"{CLAUDE_BASE_URL}/organizations"
    result = curl_request(url, cookie_header)

    # Ensure the result is a list
    if isinstance(result, list):
        return result
    return [result]


def fetch_usage(org_id: str, cookie_header: str) -> dict:
    """Fetch quota information"""
    url = f"{CLAUDE_BASE_URL}/organizations/{org_id}/usage"
    return curl_request(url, cookie_header)


def fetch_account_info(cookie_header: str) -> dict:
    """Fetch account information"""
    url = f"{CLAUDE_BASE_URL}/account"
    try:
        return curl_request(url, cookie_header)
    except ClaudeAPIError:
        return {}


def select_organization(orgs: list) -> dict:
    """Select organization"""
    if not orgs:
        raise ClaudeAPIError("Account is not associated with any organization")

    for org in orgs:
        caps = [c.lower() for c in org.get("capabilities", [])]
        if "chat" in caps:
            return org

    for org in orgs:
        caps = [c.lower() for c in org.get("capabilities", [])]
        if caps != ["api"]:
            return org

    return orgs[0]


def format_percent(percent: Optional[float]) -> str:
    """Format percentage"""
    if percent is None:
        return "N/A"
    no_color = os.environ.get('NO_COLOR') or not sys.stdout.isatty()
    if no_color:
        return f"{percent:.1f}%"
    color = "\033[32m" if percent < 50 else "\033[33m" if percent < 80 else "\033[31m"
    reset = "\033[0m"
    return f"{color}{percent:.1f}%{reset}"


def format_reset_time(resets_at: Optional[str]) -> str:
    """Format reset time to local timezone (auto-detect or use SGT)"""
    if not resets_at:
        return "N/A"
    try:
        from datetime import datetime, timezone, timedelta
        import os

        # Parse ISO 8601 time
        dt = datetime.fromisoformat(resets_at.replace("Z", "+00:00"))

        # Get local timezone, default to Singapore Time (SGT = UTC+8)
        # Can be set via TZ environment variable
        tz_str = os.environ.get("TZ", "Asia/Singapore")

        if tz_str == "Asia/Singapore" or tz_str == "SGT":
            # Singapore Time UTC+8
            local_tz = timezone(timedelta(hours=8))
            tz_name = "SGT"
        elif tz_str == "Asia/Shanghai" or tz_str == "CST":
            # China Standard Time UTC+8
            local_tz = timezone(timedelta(hours=8))
            tz_name = "CST"
        elif tz_str == "Asia/Tokyo" or tz_str == "JST":
            # Japan Standard Time UTC+9
            local_tz = timezone(timedelta(hours=9))
            tz_name = "JST"
        elif tz_str == "Asia/Seoul" or tz_str == "KST":
            # Korea Standard Time UTC+9
            local_tz = timezone(timedelta(hours=9))
            tz_name = "KST"
        else:
            # Try to use system local time
            import time
            local_offset = -time.timezone
            if time.daylight:
                local_offset += 3600
            local_tz = timezone(timedelta(seconds=local_offset))
            tz_name = "Local"

        # Convert to local timezone
        local_dt = dt.astimezone(local_tz)

        return local_dt.strftime(f"%Y-%m-%d %H:%M {tz_name}")
    except Exception:
        return resets_at


def get_system_timezone():
    """Get system local timezone"""
    from datetime import timezone as dt_timezone, timedelta as dt_timedelta
    import time
    
    try:
        # Get system local timezone offset (in seconds)
        local_offset = -time.timezone
        # Check daylight saving time
        if time.daylight and time.localtime().tm_isdst > 0:
            local_offset += 3600
        
        return dt_timezone(dt_timedelta(seconds=local_offset))
    except Exception:
        # Fall back to UTC+8 (SGT)
        return dt_timezone(dt_timedelta(hours=8))


def convert_times_to_local(data: dict) -> dict:
    """Convert all ISO times in data to local time"""
    import copy
    from datetime import datetime
    result = copy.deepcopy(data)

    def convert_value(value):
        if isinstance(value, str):
            # Try to parse ISO 8601 time
            try:
                dt = datetime.fromisoformat(value.replace("Z", "+00:00"))
                # Convert to system local timezone
                local_tz = get_system_timezone()
                local_dt = dt.astimezone(local_tz)
                # Get timezone name
                tz_name = local_dt.tzname() or "Local"
                return local_dt.strftime(f"%Y-%m-%d %H:%M:%S {tz_name}")
            except Exception:
                return value
        elif isinstance(value, dict):
            return {k: convert_value(v) for k, v in value.items()}
        elif isinstance(value, list):
            return [convert_value(item) for item in value]
        return value

    return convert_value(result)


def add_resets_in(data: dict) -> dict:
    """Add human-readable 'resets_in' field next to each 'resets_at' timestamp"""
    import copy
    from datetime import datetime, timezone
    result = copy.deepcopy(data)
    now = datetime.now(timezone.utc)

    for key, value in result.items():
        if not isinstance(value, dict):
            continue
        resets_at = value.get("resets_at")
        if not resets_at or not isinstance(resets_at, str):
            continue
        try:
            dt = datetime.fromisoformat(resets_at.replace("Z", "+00:00"))
            delta = dt - now
            total_seconds = int(delta.total_seconds())
            if total_seconds <= 0:
                value["resets_in"] = "now"
                continue
            hours, remainder = divmod(total_seconds, 3600)
            minutes = remainder // 60
            if hours > 0:
                value["resets_in"] = f"{hours}h {minutes}m"
            else:
                value["resets_in"] = f"{minutes}m"
        except Exception:
            pass

    return result


def print_usage(data: dict, show_raw: bool = False) -> None:
    """Print quota information"""
    if show_raw:
        # Add time-to-reset and convert times to local timezone
        local_data = convert_times_to_local(add_resets_in(data))
        print(json.dumps(local_data, indent=2, ensure_ascii=False))
        return

    print("\n" + "=" * 50)
    print("       Claude Code Quota Usage (Web API)")
    print("=" * 50)

    # 5-hour session quota
    five_hour = data.get("five_hour") or {}
    print(f"\n  Current Session (5-hour window)")
    if five_hour:
        util = five_hour.get("utilization")
        resets = five_hour.get("resets_at")
        remaining = 100 - util if util is not None else None
        if util is not None:
            print(f"   Used:      {format_percent(util)}")
            print(f"   Remaining: {format_percent(remaining)}")
        else:
            print(f"   Status: No data")
        if resets:
            print(f"   Reset time: {format_reset_time(resets)}")
    else:
        print(f"   Status: N/A")

    # 7-day weekly quota
    seven_day = data.get("seven_day")
    print(f"\n  Weekly Usage (7-day window)")
    if seven_day:
        util = seven_day.get("utilization")
        resets = seven_day.get("resets_at")
        remaining = 100 - util if util is not None else None
        if util is not None:
            print(f"   Used:      {format_percent(util)}")
            print(f"   Remaining: {format_percent(remaining)}")
        else:
            print(f"   Status: No data")
        if resets:
            print(f"   Reset time: {format_reset_time(resets)}")
    else:
        print(f"   Status: N/A (possibly Enterprise account or special plan)")

    # Opus model quota
    seven_day_opus = data.get("seven_day_opus")
    print(f"\n  Opus Model Quota (7-day)")
    if seven_day_opus:
        util = seven_day_opus.get("utilization")
        remaining = 100 - util if util is not None else None
        if util is not None:
            print(f"   Used:      {format_percent(util)}")
            print(f"   Remaining: {format_percent(remaining)}")
        else:
            print(f"   Status: No data")
    else:
        print(f"   Status: N/A")

    # Sonnet model quota
    seven_day_sonnet = data.get("seven_day_sonnet")
    print(f"\n  Sonnet Model Quota (7-day)")
    if seven_day_sonnet:
        util = seven_day_sonnet.get("utilization")
        remaining = 100 - util if util is not None else None
        if util is not None:
            print(f"   Used:      {format_percent(util)}")
            print(f"   Remaining: {format_percent(remaining)}")
        else:
            print(f"   Status: No data")
    else:
        print(f"   Status: N/A")

    # Extra usage credits
    extra = data.get("extra_usage")
    print(f"\n  Extra Usage Credits")
    if extra and extra.get("is_enabled"):
        used = extra.get("used_credits")
        limit = extra.get("monthly_limit")
        currency = extra.get("currency", "USD")
        util = extra.get("utilization")
        print(f"   Status: Enabled")
        if used is not None and limit is not None:
            used_dollars = used / 100.0
            limit_dollars = limit / 100.0
            print(f"   Used: {currency} ${used_dollars:.2f} / ${limit_dollars:.2f}")
        if util is not None:
            print(f"   Utilization: {format_percent(util)}")
    else:
        print(f"   Status: N/A")

    print("\n" + "=" * 50)


def spawn_background_refresh(args: argparse.Namespace) -> None:
    """Spawn a detached background process to refresh the cache"""
    cmd = [sys.executable, os.path.abspath(__file__), "--bg-refresh"]
    if args.cookie:
        cmd.extend(["--cookie", args.cookie])
    if args.profile:
        cmd.extend(["--profile", args.profile])
    if args.debounce:
        cmd.extend(["--debounce", str(args.debounce)])
    # Detach completely: redirect stdio to devnull, start new session
    try:
        subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            stdin=subprocess.DEVNULL,
            start_new_session=True,
        )
    except Exception:
        # Best-effort; if spawn fails, next call will retry
        pass


def bg_refresh_main(args: argparse.Namespace) -> int:
    """Background refresh: fetch API and update cache, then exit"""
    debounce_interval = args.debounce if args.debounce > 0 else DEBOUNCE_DEFAULT_INTERVAL
    dm = DebounceManager(interval_seconds=debounce_interval)

    if not dm.try_acquire_refresh_lock():
        # Another refresh is already running
        return 0

    try:
        if args.cookie:
            cookie_info = CookieInfo(header=args.cookie, source="Command line argument")
        else:
            cookie_info = get_cookies_from_firefox(args.profile)

        orgs = fetch_organizations(cookie_info.header)
        org = select_organization(orgs)
        usage_data = fetch_usage(org["uuid"], cookie_info.header)
        dm.record_call({"usage": usage_data})
        return 0
    except Exception:
        return 1
    finally:
        dm.release_refresh_lock()


def main():
    parser = argparse.ArgumentParser(
        description="Fetch Claude quota from Firefox cookies (using curl)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Debounce (status bar optimization):
  Used for high-frequency call scenarios like status bars, limits API request rate, uses cached data.

  Mechanism:
  - By default, API is called at most once every 60 seconds
  - When interval has not elapsed, returns the last cached result
  - When interval expires, returns stale cache immediately and refreshes in background
  - A lock file prevents concurrent background refreshes
  - Cache files are stored in /tmp/claude_quota_cache/

  Status bar usage examples:
    # Use in status bar script (limit to once per minute)
    python3 claude_quota_curl.py --debounce --profile rhhzhqpn.dev-edition-default

    # Custom interval (e.g. every 30 seconds)
    python3 claude_quota_curl.py --debounce 30 --profile rhhzhqpn.dev-edition-default

    # Force refresh (ignore debounce limit)
    python3 claude_quota_curl.py --force --profile rhhzhqpn.dev-edition-default

    # Clear cache
    python3 claude_quota_curl.py --clear-cache

Cloudflare bypass notes:
  If you encounter a "Just a moment..." page, the request was blocked by Cloudflare.

  Solution:
  1. Visit https://claude.ai in Firefox and complete the Cloudflare challenge
  2. Keep Firefox open, do not clear cookies
  3. Then immediately run this script

  If it still fails, try:
  - Install curl-impersonate: https://github.com/lwthiker/curl-impersonate
  - Or use --cookie to manually provide the full cookie header copied from browser dev tools

General usage:
  # Automatically get cookies from Firefox
  python3 claude_quota_curl.py --profile rhhzhqpn.dev-edition-default

  # Manually provide cookie header (including cf_clearance)
  python3 claude_quota_curl.py --cookie "sessionKey=xxx; cf_clearance=yyy"

  # Show raw API response
  python3 claude_quota_curl.py --raw
        """
    )

    parser.add_argument(
        "--profile",
        metavar="NAME",
        help="Specify Firefox profile name"
    )
    parser.add_argument(
        "--cookie",
        metavar="HEADER",
        help="Manually provide Cookie header (overrides Firefox)"
    )
    parser.add_argument(
        "--raw",
        action="store_true",
        help="Output raw API JSON response"
    )
    parser.add_argument(
        "--no-color",
        action="store_true",
        help="Disable colored output"
    )

    # Debounce options (for high-frequency status bar calls)
    debounce_group = parser.add_argument_group("Debounce options (for status bar usage)")
    debounce_group.add_argument(
        "--debounce",
        metavar="SECONDS",
        nargs="?",
        type=int,
        const=DEBOUNCE_DEFAULT_INTERVAL,
        default=0,
        help=f"Enable debounce mechanism to limit API call frequency (default {DEBOUNCE_DEFAULT_INTERVAL} seconds). Recommended for status bar scripts"
    )
    debounce_group.add_argument(
        "--debounce-cache-dir",
        metavar="DIR",
        default=str(DEBOUNCE_CACHE_DIR),
        help=f"Debounce cache directory (default: {DEBOUNCE_CACHE_DIR})"
    )
    debounce_group.add_argument(
        "--force",
        action="store_true",
        help="Force refresh, ignore debounce limit"
    )
    debounce_group.add_argument(
        "--clear-cache",
        action="store_true",
        help="Clear debounce cache and exit"
    )

    # Hidden flag for background refresh subprocess
    parser.add_argument("--bg-refresh", action="store_true", help=argparse.SUPPRESS)

    args = parser.parse_args()

    if args.no_color:
        os.environ['NO_COLOR'] = '1'

    # Handle --bg-refresh (internal: runs in detached subprocess)
    if args.bg_refresh:
        return bg_refresh_main(args)

    # Handle --clear-cache
    if args.clear_cache:
        dm = DebounceManager()
        dm.clear_cache()
        print("Debounce cache cleared", file=sys.stderr)
        return 0

    # Set debounce interval
    debounce_interval = args.debounce if args.debounce > 0 else DEBOUNCE_DEFAULT_INTERVAL

    try:
        # Initialize debounce manager
        dm = DebounceManager(interval_seconds=debounce_interval)

        # Check debounce
        should_skip, cached_data, remaining, needs_refresh = dm.should_skip_call()

        if should_skip and not args.force:
            if cached_data:
                if needs_refresh:
                    # Stale-while-revalidate: return stale cache, refresh in background
                    if not args.raw:
                        print(f"Returning cached data, refreshing in background", file=sys.stderr)
                    spawn_background_refresh(args)
                else:
                    if not args.raw:
                        print(f"Debounce active: {remaining:.0f} seconds until next refresh", file=sys.stderr)

                usage_data = cached_data.get("usage", {})
                print_usage(usage_data, show_raw=args.raw)
                return 0
            else:
                # No cached data, proceed with synchronous API call
                pass

        # Get cookie header
        if args.cookie:
            cookie_info = CookieInfo(
                header=args.cookie,
                source="Command line argument"
            )
        else:
            cookie_info = get_cookies_from_firefox(args.profile)

        if not args.raw:
            if should_skip and args.force:
                print(f"Force refresh (ignoring {remaining:.0f} second debounce limit)", file=sys.stderr)
            print(f"\nCookie source: {cookie_info.source}", file=sys.stderr)
            cookie_count = len(cookie_info.header.split(';'))
            print(f"   Cookies: {cookie_count}", file=sys.stderr)
            # Show sessionKey prefix
            sk_match = re.search(r'sessionKey=([^;]+)', cookie_info.header)
            if sk_match:
                sk = sk_match.group(1)
                print(f"   SessionKey: {sk[:25]}...", file=sys.stderr)

        # Fetch organization list
        orgs = fetch_organizations(cookie_info.header)
        org = select_organization(orgs)

        # Fetch quota information
        usage_data = fetch_usage(org["uuid"], cookie_info.header)

        # Record call time and result (for debounce)
        result_to_cache = {
            "usage": usage_data
        }
        dm.record_call(result_to_cache)

        # Output result
        print_usage(usage_data, show_raw=args.raw)

        return 0

    except ClaudeAuthError as e:
        print(f"Auth error: {e}", file=sys.stderr)
        return 1
    except ClaudeAPIError as e:
        print(f"API error: {e}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("\nCancelled", file=sys.stderr)
        return 130
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
