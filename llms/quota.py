#!/usr/bin/env python3
"""
Multi-Provider Quota Checker

A clean architecture implementation for querying coding plan quotas
from different AI providers (Claude, MiniMax, etc.).

Usage:
    # Query all registered providers
    python3 quota.py --all

    # Query specific provider
    python3 quota.py --provider claude
    python3 quota.py --provider minimax

    # Raw JSON output (for scripts)
    python3 quota.py --provider claude --json

    # Specify Firefox profile
    QUOTA_PROFILE="xxx.default" python3 quota.py --provider claude

    # List available providers
    python3 quota.py --list-providers
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sqlite3
import subprocess
import sys
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Any, Callable, Optional
from zoneinfo import ZoneInfo


# ============================================================================
# Timezone Helpers
# ============================================================================


def get_local_timezone() -> ZoneInfo:
    """Get local timezone from QUOTA_TIMEZONE env var or system auto-detection.

    Priority:
      1. QUOTA_TIMEZONE env var (e.g. "America/Vancouver")
      2. System timezone via time.tzname / /etc/localtime
    """
    env_tz = os.environ.get("QUOTA_TIMEZONE")
    if env_tz:
        return ZoneInfo(env_tz)

    # Auto-detect: on macOS/Linux, tzname[0] is usually an IANA key
    try:
        tz_name = time.tzname[0]
        # Some systems return abbreviations like "PST" which aren't valid IANA keys.
        # ZoneInfo will raise KeyError for those, and we fall back below.
        return ZoneInfo(tz_name)
    except (KeyError, IndexError):
        pass

    # Fallback: read /etc/localtime symlink (macOS / Linux)
    try:
        link = os.readlink("/etc/localtime")
        # e.g. /var/db/timezone/zoneinfo/America/Vancouver
        idx = link.find("zoneinfo/")
        if idx != -1:
            return ZoneInfo(link[idx + len("zoneinfo/"):])
    except (OSError, KeyError):
        pass

    # Last resort: compute a fixed-offset timezone
    local_offset = -time.timezone
    if time.daylight and time.localtime().tm_isdst > 0:
        local_offset += 3600
    return timezone(timedelta(seconds=local_offset))


def fmt_timestamp(timestamp: str | None) -> str:
    """Convert an ISO-8601 / Z-suffixed timestamp to local time display.

    Returns 'YYYY-MM-DD hh:mm:ss AM/PM TZ' or 'not set'.
    """
    if not timestamp:
        return "not set"
    parsed = datetime.fromisoformat(timestamp.replace("Z", "+00:00"))
    return parsed.astimezone(get_local_timezone()).strftime("%Y-%m-%d %I:%M:%S %p %Z")


# ============================================================================
# Domain Models - Unified Quota Data Structures
# ============================================================================


@dataclass
class SessionQuota:
    """Represents a time-windowed quota (e.g., 5-hour session, weekly)"""

    used_percent: Optional[float] = None
    remaining_percent: Optional[float] = None
    resets_at: Optional[str] = None  # ISO 8601 timestamp
    resets_at_local: Optional[str] = None  # Local time HH:MM
    resets_in: Optional[str] = None  # Human readable duration
    limit: Optional[float] = None  # Optional: absolute limit
    used: Optional[float] = None  # Optional: absolute usage
    currency: Optional[str] = None  # Optional: for paid quotas
    is_enabled: Optional[bool] = None  # Optional: feature flag
    extra: dict[str, Any] = field(default_factory=dict)  # Provider-specific data

    def to_dict(self) -> dict[str, Any]:
        result = {}
        if self.used_percent is not None:
            result["used_percent"] = self.used_percent
        if self.remaining_percent is not None:
            result["remaining_percent"] = self.remaining_percent
        if self.resets_at:
            result["resets_at"] = self.resets_at
        if self.resets_at_local:
            result["resets_at_local"] = self.resets_at_local
        if self.resets_in:
            result["resets_in"] = self.resets_in
        if self.limit is not None:
            result["limit"] = self.limit
        if self.used is not None:
            result["used"] = self.used
        if self.currency:
            result["currency"] = self.currency
        if self.is_enabled is not None:
            result["is_enabled"] = self.is_enabled
        if self.extra:
            result["extra"] = self.extra
        return result

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "SessionQuota":
        """Create SessionQuota from dictionary"""
        return cls(
            used_percent=data.get("used_percent"),
            remaining_percent=data.get("remaining_percent"),
            resets_at=data.get("resets_at"),
            resets_at_local=data.get("resets_at_local"),
            resets_in=data.get("resets_in"),
            limit=data.get("limit"),
            used=data.get("used"),
            currency=data.get("currency"),
            is_enabled=data.get("is_enabled"),
            extra=data.get("extra", {}),
        )


@dataclass
class ResetCredit:
    """Represents a provider-issued rate-limit reset credit."""

    id: Optional[str] = None
    reset_type: Optional[str] = None
    status: Optional[str] = None
    granted_at: Optional[str] = None
    expires_at: Optional[str] = None
    expires_at_local: Optional[str] = None
    expires_in: Optional[str] = None
    redeem_started_at: Optional[str] = None
    redeemed_at: Optional[str] = None

    def to_dict(self) -> dict[str, Any]:
        result = {}
        if self.id:
            result["id"] = self.id
        if self.reset_type:
            result["reset_type"] = self.reset_type
        if self.status:
            result["status"] = self.status
        if self.granted_at:
            result["granted_at"] = self.granted_at
        if self.expires_at:
            result["expires_at"] = self.expires_at
        if self.expires_at_local:
            result["expires_at_local"] = self.expires_at_local
        if self.expires_in:
            result["expires_in"] = self.expires_in
        if self.redeem_started_at:
            result["redeem_started_at"] = self.redeem_started_at
        if self.redeemed_at:
            result["redeemed_at"] = self.redeemed_at
        return result

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "ResetCredit":
        """Create ResetCredit from dictionary"""
        return cls(
            id=data.get("id"),
            reset_type=data.get("reset_type"),
            status=data.get("status"),
            granted_at=data.get("granted_at"),
            expires_at=data.get("expires_at"),
            expires_at_local=data.get("expires_at_local"),
            expires_in=data.get("expires_in"),
            redeem_started_at=data.get("redeem_started_at"),
            redeemed_at=data.get("redeemed_at"),
        )


@dataclass
class QuotaInfo:
    """Unified quota information from a provider"""

    provider: str
    account: Optional[str] = None
    plan_type: Optional[str] = None  # e.g., "free", "pro", "enterprise"
    sessions: dict[str, SessionQuota] = field(default_factory=dict)
    # Common session keys: "current" (5-hour), "weekly", "weekly_opus", "weekly_sonnet"
    reset_credit_count: Optional[int] = None
    reset_credits: list[ResetCredit] = field(default_factory=list)
    reset_credits_error: Optional[str] = None
    raw_response: Optional[dict[str, Any]] = None
    error: Optional[str] = None
    # "client" = transient (curl/network failure, should retry)
    # "server" = API returned an error response (should cache normally)
    error_type: Optional[str] = None
    fetched_at: Optional[str] = None

    def to_dict(self) -> dict[str, Any]:
        result = {
            "provider": self.provider,
        }
        if self.account:
            result["account"] = self.account
        if self.plan_type:
            result["plan_type"] = self.plan_type
        if self.sessions:
            result["sessions"] = {k: v.to_dict() for k, v in self.sessions.items()}
        if self.reset_credit_count is not None:
            result["reset_credit_count"] = self.reset_credit_count
        if self.reset_credits or self.reset_credit_count is not None:
            result["reset_credits"] = [credit.to_dict() for credit in self.reset_credits]
        if self.reset_credits_error:
            result["reset_credits_error"] = self.reset_credits_error
        if self.raw_response:
            result["raw_response"] = self.raw_response
        if self.error:
            result["error"] = self.error
        if self.error_type:
            result["error_type"] = self.error_type
        if self.fetched_at:
            result["fetched_at"] = self.fetched_at
        return result

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "QuotaInfo":
        """Create QuotaInfo from dictionary (for cache reconstruction)"""
        sessions = {}
        if "sessions" in data:
            for key, val in data["sessions"].items():
                if isinstance(val, dict):
                    sessions[key] = SessionQuota.from_dict(val)
                elif isinstance(val, SessionQuota):
                    sessions[key] = val

        reset_credits = []
        if "reset_credits" in data:
            for val in data["reset_credits"]:
                if isinstance(val, dict):
                    reset_credits.append(ResetCredit.from_dict(val))
                elif isinstance(val, ResetCredit):
                    reset_credits.append(val)

        return cls(
            provider=data.get("provider", "unknown"),
            account=data.get("account"),
            plan_type=data.get("plan_type"),
            sessions=sessions,
            reset_credit_count=data.get("reset_credit_count"),
            reset_credits=reset_credits,
            reset_credits_error=data.get("reset_credits_error"),
            raw_response=data.get("raw_response"),
            error=data.get("error"),
            error_type=data.get("error_type"),
            fetched_at=data.get("fetched_at"),
        )


# ============================================================================
# Provider Interface - Abstract Base Class
# ============================================================================


class QuotaProvider(ABC):
    """Abstract base class for quota providers"""

    name: str = "base"
    description: str = "Base provider"

    def __init__(self, cookie_header: str):
        self.cookie_header = cookie_header

    @abstractmethod
    def fetch_quota(self) -> QuotaInfo:
        """Fetch quota information from the provider"""
        pass

    @abstractmethod
    def get_api_endpoints(self) -> list[str]:
        """Return list of API endpoints used (for debugging)"""
        pass


# ============================================================================
# Claude Provider Implementation
# ============================================================================


class ClaudeQuotaProvider(QuotaProvider):
    """Claude Code quota provider (OAuth token preferred, cookie fallback)"""

    name = "claude"
    description = "Claude Code (claude.ai)"
    BASE_URL = "https://claude.ai/api"
    OAUTH_URL = "https://api.anthropic.com/api/oauth/usage"

    def __init__(self, cookie_header: str, token_manager: Optional["TokenManager"] = None):
        super().__init__(cookie_header)
        self.token_manager = token_manager

    def get_api_endpoints(self) -> list[str]:
        return [
            f"{self.BASE_URL}/organizations",
            f"{self.BASE_URL}/organizations/{{org_id}}/usage",
        ]

    def _curl_request(self, url: str, timeout: int = 30) -> dict:
        """Send request using curl"""
        cmd = [
            "curl",
            "-s",
            "-L",
            "--http1.1",
            "-H", f"Cookie: {self.cookie_header}",
            "-H", "Accept: application/json",
            "-H", "Accept-Language: en-US,en;q=0.9",
            "-H", "Referer: https://claude.ai/",
            "-H", "Origin: https://claude.ai",
            "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            "--compressed",
            "--max-time", str(timeout),
            url,
        ]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            title_match = re.search(r"<title>(.*?)</title>", text, re.IGNORECASE)
            title = title_match.group(1) if title_match else "Unknown"
            if "Just a moment" in text or "challenge" in text.lower():
                raise Exception(f"Cloudflare challenge: {title}")
            elif "login" in text.lower() or "sign in" in text.lower():
                raise Exception(f"Login required: {title}")
            else:
                raise Exception(f"API returned HTML: {title}")

        return json.loads(text)

    def _select_organization(self, orgs: list) -> dict:
        """Select the appropriate organization"""
        if not orgs:
            raise Exception("No organizations found")

        for org in orgs:
            caps = [c.lower() for c in org.get("capabilities", [])]
            if "chat" in caps:
                return org

        for org in orgs:
            caps = [c.lower() for c in org.get("capabilities", [])]
            if caps != ["api"]:
                return org

        return orgs[0]

    def _parse_session_quota(self, data: dict) -> Optional[SessionQuota]:
        """Parse session quota from Claude API response"""
        if not data:
            return None

        utilization = data.get("utilization")
        resets_at = data.get("resets_at")

        quota = SessionQuota(
            used_percent=utilization,
            remaining_percent=100 - utilization if utilization is not None else None,
            resets_at=resets_at,
        )

        if resets_at:
            quota.resets_at_local, quota.resets_in = self._format_reset_time(resets_at)

        return quota

    def _format_reset_time(self, resets_at: str) -> tuple[Optional[str], Optional[str]]:
        """Format reset time to local timezone and human-readable duration"""
        try:
            dt = datetime.fromisoformat(resets_at.replace("Z", "+00:00"))
            local_tz = get_local_timezone()
            local_dt = dt.astimezone(local_tz)

            local_str = local_dt.strftime("%H:%M")

            now = datetime.now(timezone.utc)
            delta = dt - now
            total_seconds = int(delta.total_seconds())
            duration_str = format_duration(total_seconds, local_dt)

            return local_str, duration_str
        except Exception:
            return None, None



    def _parse_usage_data(self, usage_data: dict, quota_info: QuotaInfo) -> None:
        """Parse usage response into quota_info sessions"""
        if five_hour := usage_data.get("five_hour"):
            if session := self._parse_session_quota(five_hour):
                quota_info.sessions["current"] = session

        if seven_day := usage_data.get("seven_day"):
            if session := self._parse_session_quota(seven_day):
                quota_info.sessions["weekly"] = session

        if seven_day_opus := usage_data.get("seven_day_opus"):
            if session := self._parse_session_quota(seven_day_opus):
                quota_info.sessions["weekly_opus"] = session

        if seven_day_sonnet := usage_data.get("seven_day_sonnet"):
            if session := self._parse_session_quota(seven_day_sonnet):
                quota_info.sessions["weekly_sonnet"] = session

        if extra := usage_data.get("extra_usage"):
            if extra.get("is_enabled"):
                used = extra.get("used_credits")
                limit = extra.get("monthly_limit")
                currency = extra.get("currency", "USD")
                utilization = extra.get("utilization")

                quota_info.sessions["extra_credits"] = SessionQuota(
                    used=used / 100.0 if used else None,
                    limit=limit / 100.0 if limit else None,
                    used_percent=utilization,
                    remaining_percent=100 - utilization if utilization else None,
                    currency=currency,
                    is_enabled=True,
                )

    def _oauth_request(self, token: str) -> dict:
        """Send OAuth-authenticated request to the Anthropic API."""
        cmd = [
            "curl", "-s", "-L", "--http1.1",
            "-H", f"Authorization: Bearer {token}",
            "-H", "anthropic-beta: oauth-2025-04-20",
            "-H", "Accept: application/json",
            "-H", "User-Agent: claude-cli",
            "--compressed",
            "--max-time", "10",
            self.OAUTH_URL,
        ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            raise Exception(f"OAuth curl failed: {result.stderr}")
        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            raise Exception("OAuth API returned HTML — token may be invalid")
        return json.loads(text)

    def _fetch_oauth(self) -> Optional[QuotaInfo]:
        """Fetch Claude quota via OAuth Bearer token.

        Returns QuotaInfo on success, or None if OAuth is not available.
        Raises on transient errors (network, etc.) so caller can fall back.
        """
        if not self.token_manager or not self.token_manager.has_tokens("claude"):
            return None

        token = self.token_manager.get_valid_token("claude")
        if not token:
            return None

        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            data = self._oauth_request(token)
        except Exception:
            # 401 / auth error: force refresh and retry once
            new_token = self.token_manager.force_refresh("claude")
            if not new_token:
                raise
            data = self._oauth_request(new_token)

        quota_info.raw_response = data

        if error := data.get("error"):
            error_msg = error.get("message") if isinstance(error, dict) else str(error)
            quota_info.error = error_msg
            quota_info.error_type = "server"
            return quota_info

        self._parse_usage_data(data, quota_info)
        return quota_info

    def _fetch_cookie(self) -> QuotaInfo:
        """Fetch Claude quota via cookie-based web API (fallback)."""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            orgs_data = self._curl_request(f"{self.BASE_URL}/organizations")
            orgs = orgs_data if isinstance(orgs_data, list) else [orgs_data]
            org = self._select_organization(orgs)

            usage_data = self._curl_request(
                f"{self.BASE_URL}/organizations/{org['uuid']}/usage"
            )

            quota_info.raw_response = usage_data

            if error := usage_data.get("error"):
                error_msg = error.get("message") if isinstance(error, dict) else str(error)
                quota_info.error = error_msg
                quota_info.error_type = "server"
                return quota_info

            self._parse_usage_data(usage_data, quota_info)

        except Exception as e:
            quota_info.error = str(e)
            quota_info.error_type = "client"

        return quota_info

    def fetch_quota(self) -> QuotaInfo:
        """Fetch Claude quota: try OAuth first, fall back to cookie-based auth."""
        if self.token_manager and self.token_manager.has_tokens("claude"):
            try:
                result = self._fetch_oauth()
                if result is not None:
                    return result
            except Exception:
                pass  # Fall through to cookie-based auth
        return self._fetch_cookie()


# ============================================================================
# MiniMax Provider Implementation
# ============================================================================


class MiniMaxQuotaProvider(QuotaProvider):
    """MiniMax coding plan quota provider"""

    name = "minimax"
    description = "MiniMax (minimaxi.com)"
    API_URL = "https://www.minimaxi.com/v1/api/openplatform/coding_plan/remains"

    def __init__(self, cookie_header: str):
        super().__init__(cookie_header)

    def get_api_endpoints(self) -> list[str]:
        return [self.API_URL]

    def _curl_request(self, url: str, timeout: int = 30) -> dict:
        """Send request using curl with cookie authentication"""
        cmd = [
            "curl",
            "-s",
            "-L",
            "--http1.1",
            "-H", f"Cookie: {self.cookie_header}",
            "-H", "Content-Type: application/json",
            "-H", "Accept: application/json",
            "-H", "Accept-Language: en-US,en;q=0.9",
            "-H", "Referer: https://www.minimaxi.com/",
            "-H", "Origin: https://www.minimaxi.com",
            "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            "--compressed",
            "--max-time", str(timeout),
            url,
        ]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            raise Exception("API returned HTML page, authentication may be required")

        return json.loads(text)

    def fetch_quota(self) -> QuotaInfo:
        """Fetch MiniMax quota"""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            data = self._curl_request(self.API_URL)
            quota_info.raw_response = data

            # Parse MiniMax response structure
            if data.get("base_resp", {}).get("status_code") == 0:
                model_remains = data.get("model_remains", [])

                for model in model_remains:
                    model_name = model.get("model_name", "unknown")
                    # Use model name as session key (keep original for display)
                    session_key = model_name

                    total = model.get("current_interval_total_count", 0)
                    remaining = model.get("current_interval_usage_count", 0)
                    remains_ms = model.get("remains_time", 0)
                    end_time_ms = model.get("end_time", 0)

                    # current_interval_usage_count is the REMAINING count
                    # (API is coding_plan/remains), actual used = total - remaining
                    used = total - remaining if total > 0 else 0
                    used_percent = (used / total * 100) if total > 0 else None
                    remaining_percent = (remaining / total * 100) if total > 0 else None

                    # Convert remains_time from milliseconds to seconds
                    remains_seconds = remains_ms / 1000 if remains_ms else None

                    # Convert end_time to ISO format
                    resets_at = None
                    resets_at_local = None
                    resets_in = None
                    if end_time_ms > 0:
                        dt = datetime.fromtimestamp(end_time_ms / 1000, tz=timezone.utc)
                        resets_at = dt.isoformat()

                        # Local time
                        local_tz = get_local_timezone()
                        local_dt = dt.astimezone(local_tz)
                        resets_at_local = local_dt.strftime("%H:%M")

                        # Time until reset
                        now = datetime.now(timezone.utc)
                        delta = dt - now
                        total_seconds = int(delta.total_seconds())
                        resets_in = format_duration(total_seconds, local_dt)

                    quota_info.sessions[session_key] = SessionQuota(
                        used_percent=used_percent,
                        remaining_percent=remaining_percent,
                        resets_at=resets_at,
                        resets_at_local=resets_at_local,
                        resets_in=resets_in,
                        limit=total,
                        used=used,  # actual used = total - remaining
                        extra={
                            "model_name": model_name,
                            "remains_seconds": remains_seconds,
                        },
                    )
            else:
                error_msg = data.get("base_resp", {}).get("status_msg", "Unknown error")
                quota_info.error = error_msg
                quota_info.error_type = "server"

        except Exception as e:
            quota_info.error = str(e)
            quota_info.error_type = "client"

        return quota_info




# ============================================================================
# GLM Provider Implementation
# ============================================================================


class GLMQuotaProvider(QuotaProvider):
    """GLM (Zhipu AI) coding plan quota provider"""

    name = "glm"
    description = "GLM Coding Plan (bigmodel.cn)"
    API_URL = "https://www.bigmodel.cn/api/monitor/usage/quota/limit"

    def __init__(self, cookie_header: str):
        super().__init__(cookie_header)
        # Extract JWT token from cookies for Authorization header
        self.jwt_token = self._extract_jwt_token(cookie_header)

    def _extract_jwt_token(self, cookie_header: str) -> Optional[str]:
        """Extract bigmodel_token_production JWT from cookie header"""
        for part in cookie_header.split(";"):
            part = part.strip()
            if part.startswith("bigmodel_token_production="):
                return part.split("=", 1)[1]
        return None

    def get_api_endpoints(self) -> list[str]:
        return [self.API_URL]

    def _curl_request(self, url: str, timeout: int = 30) -> dict:
        """Send request using curl with cookie and JWT authentication"""
        headers = [
            "-H", f"Cookie: {self.cookie_header}",
            "-H", "Accept: application/json, text/plain, */*",
            "-H", "Accept-Language: en",
            "-H", "Accept-Encoding: gzip, deflate, br, zstd",
            "-H", "Referer: https://www.bigmodel.cn/usercenter/glm-coding/usage",
            "-H", "Set-Language: en",
            "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:149.0) Gecko/20100101 Firefox/149.0",
        ]

        # Add Authorization header with JWT token if available
        if self.jwt_token:
            headers.extend(["-H", f"Authorization: Bearer {self.jwt_token}"])

        cmd = [
            "curl",
            "-s",
            "-L",
            "--http1.1",
            *headers,
            "--compressed",
            "--max-time", str(timeout),
            url,
        ]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if not text:
            raise Exception("API returned empty response (token may be expired)")

        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            raise Exception("API returned HTML page, authentication may be required")

        return json.loads(text)

    def fetch_quota(self) -> QuotaInfo:
        """Fetch GLM quota"""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            data = self._curl_request(self.API_URL)
            quota_info.raw_response = data

            # Parse GLM response structure
            # Response format: {"code": 200, "data": {"limits": [...], "level": "..."}, "msg": "..."}
            if data.get("code") == 200:
                quota_data = data.get("data", {})
                limits = quota_data.get("limits", [])

                for limit in limits:
                    limit_type = limit.get("type", "")
                    unit = limit.get("unit")
                    number = limit.get("number")
                    percentage = limit.get("percentage", 0)
                    next_reset_ms = limit.get("nextResetTime", 0)

                    # Calculate reset time
                    resets_at = None
                    resets_at_local = None
                    resets_in = None
                    if next_reset_ms > 0:
                        dt = datetime.fromtimestamp(next_reset_ms / 1000, tz=timezone.utc)
                        resets_at = dt.isoformat()
                        local_tz = get_local_timezone()
                        local_dt = dt.astimezone(local_tz)
                        resets_at_local = local_dt.strftime("%H:%M")
                        now = datetime.now(timezone.utc)
                        delta = dt - now
                        total_seconds = int(delta.total_seconds())
                        resets_in = format_duration(total_seconds, local_dt)

                    if limit_type == "TIME_LIMIT":
                        # TIME_LIMIT is the MCP Monthly Quota
                        # usage = total prompts, remaining = remaining prompts
                        total = limit.get("usage", 0)
                        remaining_prompts = limit.get("remaining", 0)
                        used_prompts = total - remaining_prompts

                        quota_info.sessions["mcp_monthly"] = SessionQuota(
                            used_percent=percentage,
                            remaining_percent=100 - percentage,
                            used=used_prompts,
                            limit=total,
                            resets_at=resets_at,
                            resets_at_local=resets_at_local,
                            resets_in=resets_in,
                            extra={
                                "unit": unit,
                                "number": number,
                                "remaining_prompts": remaining_prompts,
                                "usage_details": limit.get("usageDetails", []),
                            },
                        )

                    elif limit_type == "TOKENS_LIMIT":
                        # TOKENS_LIMIT is actually a time-based session limit
                        # unit=3: 5-hour session (resets frequently)
                        # unit=6: weekly session
                        if unit == 3:
                            session_key = "current"  # 5-hour session
                        elif unit == 6:
                            session_key = "weekly"
                        else:
                            session_key = f"session_{unit}"

                        quota_info.sessions[session_key] = SessionQuota(
                            used_percent=percentage,
                            remaining_percent=100 - percentage,
                            resets_at=resets_at,
                            resets_at_local=resets_at_local,
                            resets_in=resets_in,
                            extra={
                                "unit": unit,
                                "number": number,
                            },
                        )

                # Set plan level
                quota_info.plan_type = quota_data.get("level")

            else:
                error_msg = data.get("msg") or data.get("message") or "Unknown error"
                quota_info.error = error_msg
                quota_info.error_type = "server"

        except Exception as e:
            quota_info.error = str(e)
            quota_info.error_type = "client"

        return quota_info




# ============================================================================
# Kimi Provider Implementation
# ============================================================================


class CodexQuotaProvider(QuotaProvider):
    """Codex (ChatGPT API) quota provider — supports OAuth token refresh"""

    name = "codex"
    description = "Codex (chatgpt.com)"
    API_URL = "https://chatgpt.com/backend-api/wham/usage"
    RESET_CREDITS_URL = "https://chatgpt.com/backend-api/wham/rate-limit-reset-credits"

    def __init__(self, auth_token: str, account_id: str, token_manager: Optional["TokenManager"] = None):
        super().__init__("")
        self.auth_token = auth_token
        self.account_id = account_id
        self.token_manager = token_manager

    def _refresh_token_if_needed(self) -> bool:
        """Try to refresh the auth token via TokenManager. Returns True if refreshed."""
        if not self.token_manager:
            return False
        new_token = self.token_manager.force_refresh("codex")
        if new_token:
            self.auth_token = new_token
            # Also update account_id if available
            acct = self.token_manager.get_account_id("codex")
            if acct:
                self.account_id = acct
            return True
        return False

    def get_api_endpoints(self) -> list[str]:
        return [self.API_URL, self.RESET_CREDITS_URL]

    @classmethod
    def from_codex_config(cls, token_manager: Optional["TokenManager"] = None) -> "CodexQuotaProvider":
        """Create provider from Codex config files (~/.codex/auth.json).

        Falls back to TokenManager if auth.json is not found but OAuth tokens exist.
        """
        codex_dir = Path.home() / ".codex"

        # Read auth token
        auth_file = codex_dir / "auth.json"
        if not auth_file.exists():
            # Fallback to TokenManager OAuth tokens
            if token_manager and token_manager.has_tokens("codex"):
                token = token_manager.get_valid_token("codex")
                account_id = token_manager.get_account_id("codex")
                if token and account_id:
                    return cls(token, account_id, token_manager=token_manager)
            raise Exception(f"Codex auth file not found: {auth_file}")

        auth_data = json.loads(auth_file.read_text())

        # Try different formats:
        # 1. {"tokens": {"access_token": "...", "account_id": "..."}}
        # 2. {"access_token": "...", "account_id": "..."}
        # 3. {"access_token": "...", "expires_at": ...}

        access_token = None
        account_id = None

        # Format 1: tokens key
        if "tokens" in auth_data:
            tokens = auth_data.get("tokens", {})
            access_token = tokens.get("access_token")
            account_id = tokens.get("account_id")

        # Format 2: direct keys
        if not access_token:
            access_token = auth_data.get("access_token")
        if not account_id:
            account_id = auth_data.get("account_id")

        if not access_token:
            raise Exception("No access_token found in Codex auth file")

        # Try to get account_id from token payload if not found
        if not account_id:
            try:
                import base64
                parts = access_token.split(".")
                if len(parts) >= 2:
                    payload = parts[1]
                    # Add padding if needed
                    padding = 4 - len(payload) % 4
                    if padding != 4:
                        payload += "=" * padding
                    data = json.loads(base64.b64decode(payload))
                    account_id = data.get("chatgpt_account_id")
            except Exception:
                pass

        if not account_id:
            raise Exception("No account_id found in Codex auth file")

        return cls(access_token, account_id, token_manager=token_manager)

    def _curl_request(self, url: str, timeout: int = 30, extra_headers: Optional[list[str]] = None) -> dict:
        """Send request using curl with Bearer auth"""
        headers = [
            "-H",
            f"Authorization: Bearer {self.auth_token}",
            "-H",
            f"ChatGPT-Account-ID: {self.account_id}",
            "-H",
            "Accept: application/json",
            "-H",
            "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
        ]
        for header in extra_headers or []:
            headers.extend(["-H", header])

        cmd = ["curl", "-s", "-L", "--http1.1", *headers, "--max-time", str(timeout), url]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            raise Exception(f"API returned HTML: {text[:200]}")

        return json.loads(text)

    def _parse_reset_time(self, reset_at: int) -> tuple[Optional[str], Optional[str], Optional[str]]:
        """Parse reset timestamp (Unix) and calculate local time and duration"""
        if not reset_at:
            return None, None, None

        try:
            dt = datetime.fromtimestamp(reset_at, tz=timezone.utc)
            resets_at = dt.isoformat()

            # Local time
            local_tz = get_local_timezone()
            local_dt = dt.astimezone(local_tz)
            resets_at_local = local_dt.strftime("%H:%M")

            # Time until reset
            now = datetime.now(timezone.utc)
            delta = dt - now
            total_seconds = int(delta.total_seconds())
            resets_in = format_duration(total_seconds, local_dt)

            return resets_at, resets_at_local, resets_in
        except Exception:
            return None, None, None

    def _parse_iso_expiry(self, expires_at: Optional[str]) -> tuple[Optional[str], Optional[str]]:
        """Parse an ISO expiry timestamp into local time and duration."""
        if not expires_at:
            return None, None

        try:
            dt = datetime.fromisoformat(expires_at.replace("Z", "+00:00"))
            if dt.tzinfo is None:
                dt = dt.replace(tzinfo=timezone.utc)

            local_tz = get_local_timezone()
            local_dt = dt.astimezone(local_tz)
            expires_at_local = local_dt.strftime("%H:%M")

            now = datetime.now(timezone.utc)
            total_seconds = int((dt.astimezone(timezone.utc) - now).total_seconds())
            expires_in = format_duration(total_seconds)

            return expires_at_local, expires_in
        except (TypeError, ValueError):
            return None, None

    def _parse_reset_credit(self, credit: dict[str, Any]) -> ResetCredit:
        """Parse a Codex reset credit response item."""
        expires_at = credit.get("expires_at")
        expires_at_local, expires_in = self._parse_iso_expiry(expires_at)

        return ResetCredit(
            id=credit.get("id"),
            reset_type=credit.get("reset_type"),
            status=credit.get("status"),
            granted_at=credit.get("granted_at"),
            expires_at=expires_at,
            expires_at_local=expires_at_local,
            expires_in=expires_in,
            redeem_started_at=credit.get("redeem_started_at"),
            redeemed_at=credit.get("redeemed_at"),
        )

    def _parse_reset_credit_count(self, value: Any) -> Optional[int]:
        """Parse reset credit count from either usage or detail responses."""
        if value is None:
            return None
        try:
            return int(value)
        except (TypeError, ValueError):
            return None

    def _apply_reset_credits(self, quota_info: QuotaInfo, data: dict[str, Any]) -> None:
        """Merge Codex reset credit detail response into quota info."""
        count = self._parse_reset_credit_count(data.get("available_count"))
        if count is not None:
            quota_info.reset_credit_count = count

        credits = data.get("credits", [])
        if isinstance(credits, list):
            quota_info.reset_credits = [
                self._parse_reset_credit(credit)
                for credit in credits
                if isinstance(credit, dict)
            ]

        if quota_info.reset_credit_count is None and quota_info.reset_credits:
            quota_info.reset_credit_count = sum(
                1 for credit in quota_info.reset_credits if credit.status == "available"
            )



    def _do_fetch_api(self, quota_info: QuotaInfo) -> None:
        """Core API fetching logic (extracted for retry on auth failure)."""
        data = self._curl_request(self.API_URL)
        quota_info.raw_response = data

        # Parse Codex response structure
        quota_info.plan_type = data.get("plan_type")
        quota_info.account = data.get("email")

        reset_summary = data.get("rate_limit_reset_credits", {})
        if isinstance(reset_summary, dict):
            count = self._parse_reset_credit_count(reset_summary.get("available_count"))
            if count is not None:
                quota_info.reset_credit_count = count

        # Parse rate_limit (primary = session, secondary = weekly)
        rate_limit = data.get("rate_limit", {})
        if rate_limit:

            # Primary window (session limit)
            primary = rate_limit.get("primary_window", {})
            if primary and not rate_limit.get("limit_reached", False):
                used_percent = primary.get("used_percent")
                reset_at = primary.get("reset_at", 0)
                resets_at, resets_at_local, resets_in = self._parse_reset_time(reset_at)

                quota_info.sessions["current"] = SessionQuota(
                    used_percent=used_percent,
                    remaining_percent=100 - used_percent if used_percent is not None else None,
                    resets_at=resets_at,
                    resets_at_local=resets_at_local,
                    resets_in=resets_in,
                    extra={
                        "limit_window_seconds": primary.get("limit_window_seconds"),
                    },
                )

            # Secondary window (weekly)
            secondary = rate_limit.get("secondary_window")
            if secondary and not rate_limit.get("limit_reached", False):
                used_percent = secondary.get("used_percent")
                reset_at = secondary.get("reset_at", 0)
                resets_at, resets_at_local, resets_in = self._parse_reset_time(reset_at)

                quota_info.sessions["weekly"] = SessionQuota(
                    used_percent=used_percent,
                    remaining_percent=100 - used_percent if used_percent is not None else None,
                    resets_at=resets_at,
                    resets_at_local=resets_at_local,
                    resets_in=resets_in,
                    extra={
                        "limit_window_seconds": secondary.get("limit_window_seconds"),
                    },
                )

        try:
            reset_credit_data = self._curl_request(
                self.RESET_CREDITS_URL,
                extra_headers=[
                    "OpenAI-Beta: codex-1",
                    "Originator: Codex Desktop",
                ],
            )
            raw_error = reset_credit_data.get("error")
            if raw_error:
                message = raw_error.get("message") if isinstance(raw_error, dict) else str(raw_error)
                raise Exception(message)
            self._apply_reset_credits(quota_info, reset_credit_data)
        except Exception as e:
            quota_info.reset_credits_error = str(e)

    def fetch_quota(self) -> QuotaInfo:
        """Fetch Codex quota with auto token-refresh on auth failure."""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            self._do_fetch_api(quota_info)
        except Exception as e:
            # Try token refresh + retry once
            if self._refresh_token_if_needed():
                try:
                    quota_info = QuotaInfo(
                        provider=self.name,
                        fetched_at=datetime.now(timezone.utc).isoformat(),
                    )
                    self._do_fetch_api(quota_info)
                    return quota_info
                except Exception:
                    pass
            quota_info.error = str(e)
            quota_info.error_type = "client"

        return quota_info


class KimiQuotaProvider(QuotaProvider):
    """Kimi coding plan quota provider"""

    name = "kimi"
    description = "Kimi Coding Plan (kimi.com)"
    API_URL = "https://www.kimi.com/apiv2/kimi.gateway.billing.v1.BillingService/GetUsages"

    def __init__(self, cookie_header: str):
        super().__init__(cookie_header)
        # Extract JWT token from kimi-auth cookie for Authorization header
        self.jwt_token = self._extract_jwt_token(cookie_header)

    def _extract_jwt_token(self, cookie_header: str) -> Optional[str]:
        """Extract kimi-auth JWT from cookie header"""
        for part in cookie_header.split(";"):
            part = part.strip()
            if part.startswith("kimi-auth="):
                return part.split("=", 1)[1]
        return None

    def get_api_endpoints(self) -> list[str]:
        return [self.API_URL]

    def _curl_request(self, url: str, timeout: int = 30) -> dict:
        """Send POST request using curl with cookie and JWT authentication"""
        headers = [
            "-H", f"Cookie: {self.cookie_header}",
            "-H", "Accept: */*",
            "-H", "Accept-Language: en-US,en;q=0.9",
            "-H", "Accept-Encoding: gzip, deflate, br, zstd",
            "-H", "Referer: https://www.kimi.com/code/console",
            "-H", "Content-Type: application/json",
            "-H", "connect-protocol-version: 1",
            "-H", "x-msh-platform: web",
            "-H", "x-msh-version: 1.0.0",
            "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:149.0) Gecko/20100101 Firefox/149.0",
        ]

        # Add Authorization header with JWT token if available
        if self.jwt_token:
            headers.extend(["-H", f"Authorization: Bearer {self.jwt_token}"])

        cmd = [
            "curl",
            "-s",
            "-L",
            "--http1.1",
            "-X", "POST",
            *headers,
            "--compressed",
            "--max-time", str(timeout),
            "-d", '{"scope":["FEATURE_CODING"]}',
            url,
        ]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            raise Exception("API returned HTML page, authentication may be required")

        return json.loads(text)

    def _parse_reset_time(self, reset_time: str) -> tuple[Optional[str], Optional[str], Optional[str]]:
        """Parse reset time and calculate local time and duration"""
        if not reset_time:
            return None, None, None

        try:
            dt = datetime.fromisoformat(reset_time.replace("Z", "+00:00"))
            resets_at = dt.isoformat()

            # Local time
            local_tz = get_local_timezone()
            local_dt = dt.astimezone(local_tz)
            resets_at_local = local_dt.strftime("%H:%M")

            # Time until reset
            now = datetime.now(timezone.utc)
            delta = dt - now
            total_seconds = int(delta.total_seconds())
            resets_in = format_duration(total_seconds, local_dt)

            return resets_at, resets_at_local, resets_in
        except Exception:
            return None, None, None



    def fetch_quota(self) -> QuotaInfo:
        """Fetch Kimi quota"""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            data = self._curl_request(self.API_URL)
            quota_info.raw_response = data

            # Detect API-level errors (e.g., expired JWT token)
            if error_code := data.get("code"):
                reason = ""
                for detail in data.get("details", []):
                    if r := detail.get("debug", {}).get("reason"):
                        reason = f": {r}"
                        break
                quota_info.error = f"{error_code}{reason}"
                quota_info.error_type = "client"
                return quota_info

            # Parse Kimi response structure
            usages = data.get("usages", [])

            for usage in usages:
                if usage.get("scope") != "FEATURE_CODING":
                    continue

                # Main quota detail - this is the weekly quota
                # Note: limit/used/remaining are PERCENTAGES, not prompt counts
                detail = usage.get("detail", {})
                if detail:
                    # These values are percentages (e.g., used: "51" means 51%)
                    used_percent = float(detail.get("used", 0))
                    reset_time = detail.get("resetTime", "")

                    resets_at, resets_at_local, resets_in = self._parse_reset_time(reset_time)

                    quota_info.sessions["weekly"] = SessionQuota(
                        used_percent=used_percent,
                        remaining_percent=100 - used_percent,
                        resets_at=resets_at,
                        resets_at_local=resets_at_local,
                        resets_in=resets_in,
                    )

                # Window-based limits (e.g., 5-hour session)
                limits = usage.get("limits", [])
                for window_limit in limits:
                    window = window_limit.get("window", {})
                    window_detail = window_limit.get("detail", {})

                    duration = window.get("duration", 0)
                    time_unit = window.get("timeUnit", "")

                    # Determine session key based on duration
                    # 300 minutes = 5 hours
                    if time_unit == "TIME_UNIT_MINUTE" and duration == 300:
                        session_key = "current"  # 5-hour session
                    elif time_unit == "TIME_UNIT_HOUR" and duration == 24:
                        session_key = "daily"
                    elif time_unit == "TIME_UNIT_DAY" and duration == 7:
                        session_key = "weekly_window"
                    else:
                        # Default to current for unknown windows
                        session_key = "current"

                    if window_detail:
                        # These values are percentages
                        # remaining: "100" means 100% remaining (0% used)
                        w_remaining_percent = float(window_detail.get("remaining", 0))
                        w_used_percent = 100 - w_remaining_percent
                        w_reset_time = window_detail.get("resetTime", "")

                        w_resets_at, w_resets_at_local, w_resets_in = self._parse_reset_time(w_reset_time)

                        quota_info.sessions[session_key] = SessionQuota(
                            used_percent=w_used_percent,
                            remaining_percent=w_remaining_percent,
                            resets_at=w_resets_at,
                            resets_at_local=w_resets_at_local,
                            resets_in=w_resets_in,
                            extra={
                                "duration": duration,
                                "time_unit": time_unit,
                            },
                        )

        except Exception as e:
            quota_info.error = str(e)
            quota_info.error_type = "client"

        return quota_info


# ============================================================================
# Doubao Provider Implementation
# ============================================================================


class DoubaoQuotaProvider(QuotaProvider):
    """Doubao (Volcengine/ByteDance) coding plan quota provider"""

    name = "doubao"
    description = "Doubao Coding Plan (volcengine.com)"
    API_URL = "https://console.volcengine.com/api/top/ark/cn-beijing/2024-01-01/GetCodingPlanUsage"

    # Path to doubao.key file (relative to this script or in llms config dir)
    KEY_FILE = Path(__file__).parent / "doubao.key"

    def __init__(self, cookie_header: str):
        super().__init__(cookie_header)
        # Try to get csrf_token from: 1) key file, 2) env var, 3) cookies
        self.csrf_token = self._load_csrf_token()
        self.x_web_id = self._extract_cookie_value("x-web-id") or self._extract_cookie_value("volcfe-uuid")

    def _load_csrf_token(self) -> Optional[str]:
        """Load CSRF token from key file, env var, or cookies"""
        # 1. Try key file
        if self.KEY_FILE.exists():
            token = self.KEY_FILE.read_text().strip()
            if token:
                return token
        # 2. Try environment variable
        env_token = os.environ.get("DOUBAO_CSRF_TOKEN")
        if env_token:
            return env_token
        # 3. Try extracting from cookies
        return self._extract_cookie_value("csrfToken") or self._extract_cookie_value("_csrfToken")

    def _extract_cookie_value(self, name: str) -> Optional[str]:
        """Extract a specific cookie value from the cookie header"""
        for part in self.cookie_header.split(";"):
            part = part.strip()
            if part.startswith(f"{name}="):
                return part.split("=", 1)[1]
        return None

    def get_api_endpoints(self) -> list[str]:
        return [self.API_URL]

    def _curl_request(self, url: str, timeout: int = 30) -> dict:
        """Send POST request using curl with cookie authentication"""
        # Build cookie header, adding csrfToken if we have it from key file
        cookie_header = self.cookie_header
        if self.csrf_token and "csrfToken=" not in cookie_header:
            cookie_header = f"{cookie_header}; csrfToken={self.csrf_token}"

        headers = [
            "-H", f"Cookie: {cookie_header}",
            "-H", "Accept: application/json, text/plain, */*",
            "-H", "Accept-Language: zh",
            "-H", "Accept-Encoding: gzip, deflate, br, zstd",
            "-H", "Content-Type: application/json",
            "-H", "Referer: https://console.volcengine.com/ark/region:ark+cn-beijing/openManagement?LLM=%7B%7D&advancedActiveKey=subscribe",
            "-H", "Origin: https://console.volcengine.com",
            "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:149.0) Gecko/20100101 Firefox/149.0",
        ]

        # Add X-Csrf-Token header if available
        if self.csrf_token:
            headers.extend(["-H", f"X-Csrf-Token: {self.csrf_token}"])

        # Add x-web-id header if available
        if self.x_web_id:
            headers.extend(["-H", f"x-web-id: {self.x_web_id}"])

        cmd = [
            "curl",
            "-s",
            "-L",
            "--http1.1",
            "-X", "POST",
            *headers,
            "--compressed",
            "--max-time", str(timeout),
            "-d", "{}",
            url,
        ]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            raise Exception("API returned HTML page, authentication may be required")

        return json.loads(text)

    def _parse_reset_time(self, timestamp: int) -> tuple[Optional[str], Optional[str], Optional[str]]:
        """Parse Unix timestamp and calculate local time and duration"""
        if not timestamp:
            return None, None, None

        try:
            dt = datetime.fromtimestamp(timestamp, tz=timezone.utc)
            resets_at = dt.isoformat()

            # Local time
            local_tz = get_local_timezone()
            local_dt = dt.astimezone(local_tz)
            resets_at_local = local_dt.strftime("%H:%M")

            # Time until reset
            now = datetime.now(timezone.utc)
            delta = dt - now
            total_seconds = int(delta.total_seconds())
            resets_in = format_duration(total_seconds, local_dt)

            return resets_at, resets_at_local, resets_in
        except Exception:
            return None, None, None



    def fetch_quota(self) -> QuotaInfo:
        """Fetch Doubao quota"""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            data = self._curl_request(self.API_URL)
            quota_info.raw_response = data

            # Parse Doubao response structure
            # Response format: {"ResponseMetadata": {...}, "Result": {"Status": "Running", "QuotaUsage": [...]}}
            result = data.get("Result", {})
            quota_info.plan_type = result.get("Status")

            quota_usage = result.get("QuotaUsage", [])

            for usage in quota_usage:
                level = usage.get("Level", "")
                percent = usage.get("Percent", 0)
                reset_timestamp = usage.get("ResetTimestamp", 0)

                resets_at, resets_at_local, resets_in = self._parse_reset_time(reset_timestamp)

                # Map level to session key
                if level == "session":
                    session_key = "current"  # 5-hour session
                elif level == "weekly":
                    session_key = "weekly"
                elif level == "monthly":
                    session_key = "monthly"
                else:
                    session_key = level

                quota_info.sessions[session_key] = SessionQuota(
                    used_percent=percent,
                    remaining_percent=100 - percent,
                    resets_at=resets_at,
                    resets_at_local=resets_at_local,
                    resets_in=resets_in,
                    extra={
                        "level": level,
                    },
                )

            # Check for errors in ResponseMetadata
            response_meta = data.get("ResponseMetadata", {})
            if "Error" in response_meta:
                error_info = response_meta["Error"]
                error_msg = error_info.get("Message", "Unknown error")
                quota_info.error = error_msg
                quota_info.error_type = "server"

        except Exception as e:
            quota_info.error = str(e)
            quota_info.error_type = "client"

        return quota_info


# ============================================================================
# DeepSeek Provider Implementation
# ============================================================================


class DeepSeekQuotaProvider(QuotaProvider):
    """DeepSeek Platform quota provider (Bearer token from Firefox localStorage)"""

    name = "deepseek"
    description = "DeepSeek Platform (platform.deepseek.com)"
    BASE_URL = "https://platform.deepseek.com/api/v0"
    AMOUNT_URL = f"{BASE_URL}/usage/amount"
    COST_URL = f"{BASE_URL}/usage/cost"

    # Origin for Firefox localStorage (used to locate the sqlite db)
    LS_KEY = "userToken"
    LS_ORIGIN = "https+++platform.deepseek.com"

    # Peak-valley pricing (starts mid-July 2026)
    PEAK_PRICING_START = datetime(2026, 7, 15, 0, 0, 0, tzinfo=timezone.utc)
    # Peak hours in UTC: 01:00-04:00 and 06:00-10:00
    PEAK_SLOTS_UTC = [
        (1, 4),   # 01:00-04:00 UTC
        (6, 10),  # 06:00-10:00 UTC
    ]

    def __init__(self, cookie_header: str):
        super().__init__(cookie_header)
        self.bearer_token = self._load_token()

    # -- Token loading: Firefox localStorage → env var → key file ----------

    @staticmethod
    def _get_firefox_profiles_dir() -> Optional[Path]:
        """Return the Firefox profiles directory for this platform."""
        import platform
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
        return None

    @classmethod
    def _extract_localstorage_token(cls) -> Optional[str]:
        """Read the userToken from Firefox localStorage for platform.deepseek.com.

        The token is stored in:
          <profile>/storage/default/https+++platform.deepseek.com/ls/data.sqlite

        under the key 'userToken' as a JSON blob: {"value":"<token>","__version":"0"}
        """
        profiles_dir = cls._get_firefox_profiles_dir()
        if not profiles_dir or not profiles_dir.exists():
            return None

        # Try each profile, newest first
        try:
            profiles = sorted(
                [p for p in profiles_dir.iterdir() if p.is_dir()],
                key=lambda p: p.stat().st_mtime,
                reverse=True,
            )
        except OSError:
            return None

        for prof in profiles:
            ls_db = prof / "storage" / "default" / cls.LS_ORIGIN / "ls" / "data.sqlite"
            if not ls_db.exists():
                continue

            token = cls._read_localstorage_value(ls_db, cls.LS_KEY)
            if token:
                return token

        return None

    @staticmethod
    def _read_localstorage_value(db_path: Path, key: str) -> Optional[str]:
        """Copy sqlite db to temp, extract a value from the 'data' table.

        The data table schema:
          CREATE TABLE data(
            key TEXT PRIMARY KEY,
            utf16_length INTEGER NOT NULL,
            conversion_type INTEGER NOT NULL,
            compression_type INTEGER NOT NULL,
            last_access_time INTEGER NOT NULL DEFAULT 0,
            value BLOB NOT NULL
          );

        Values are JSON blobs: {"value":"...","__version":"N"}
        """
        import shutil
        import tempfile

        tmp_fd, tmp_path = tempfile.mkstemp(suffix=".sqlite")
        os.close(tmp_fd)
        tmp = Path(tmp_path)
        try:
            shutil.copy2(str(db_path), str(tmp))
            conn = sqlite3.connect(str(tmp))
            try:
                row = conn.execute(
                    "SELECT value FROM data WHERE key = ?", (key,)
                ).fetchone()
                if row and row[0]:
                    blob = row[0]
                    if isinstance(blob, bytes):
                        blob = blob.decode("utf-8", errors="replace")
                    try:
                        data = json.loads(blob)
                        if isinstance(data, dict) and "value" in data:
                            return str(data["value"])
                    except (json.JSONDecodeError, TypeError):
                        # Not JSON; return raw value
                        return blob
                return None
            finally:
                conn.close()
        except (sqlite3.Error, OSError):
            return None
        finally:
            try:
                tmp.unlink(missing_ok=True)
            except OSError:
                pass

    def _load_token(self) -> Optional[str]:
        """Load Bearer token, trying multiple sources in priority order.

        1. Firefox localStorage (userToken → value) — zero-config
        2. Env var QUOTA_DEEPSEEK_TOKEN — manual override
        """
        # 1. Firefox localStorage
        token = self._extract_localstorage_token()
        if token:
            return token

        # 2. Environment variable
        return os.environ.get("QUOTA_DEEPSEEK_TOKEN")

    def get_api_endpoints(self) -> list[str]:
        return [self.AMOUNT_URL, self.COST_URL]

    def _get_local_year_month(self) -> tuple[int, int]:
        """Get current year and month in local timezone."""
        now = datetime.now(get_local_timezone())
        return now.year, now.month

    @staticmethod
    def _is_peak_hour(now_utc: datetime) -> bool:
        """Check if current UTC time falls in peak pricing window."""
        hour = now_utc.hour
        for start, end in DeepSeekQuotaProvider.PEAK_SLOTS_UTC:
            if start <= hour < end:
                return True
        return False

    @staticmethod
    def _peak_valley_status() -> dict[str, Any]:
        """Compute peak/valley status for current time.
        
        Returns dict with:
          is_peak: whether current time is in peak pricing window
          label: "Peak (2x)" or "Valley (1x)"
          note: human-readable explanation for display
          now_utc_str: current UTC time like "07:30"
          active_windows: list of active peak windows (e.g. ["06:00-10:00"])
        """
        now_utc = datetime.now(timezone.utc)
        now_utc_str = now_utc.strftime("%H:%M")
        
        if now_utc < DeepSeekQuotaProvider.PEAK_PRICING_START:
            return {
                "is_peak": False,
                "label": "Flat (pre-peak/valley)",
                "note": "Peak/valley pricing starts ~Jul 15, 2026",
                "now_utc_str": now_utc_str,
                "active_windows": [],
            }
        
        is_peak = DeepSeekQuotaProvider._is_peak_hour(now_utc)
        active_windows = []
        hour = now_utc.hour
        for start, end in DeepSeekQuotaProvider.PEAK_SLOTS_UTC:
            if start <= hour < end:
                active_windows.append(f"{start:02d}:00-{end:02d}:00")
        
        return {
            "is_peak": is_peak,
            "label": "Peak (2x)" if is_peak else "Valley (1x)",
            "note": f"Peak hours UTC: 01:00-04:00, 06:00-10:00 (UTC+8: 09:00-12:00, 14:00-18:00)",
            "now_utc_str": now_utc_str,
            "active_windows": active_windows,
        }

    def _curl_request(self, url: str, timeout: int = 30) -> dict:
        """Send GET request with Bearer token + cookies."""
        headers = [
            "-H", f"Cookie: {self.cookie_header}",
            "-H", "Accept: */*",
            "-H", "Accept-Language: en-US,en;q=0.9",
            "-H", "Accept-Encoding: gzip, deflate, br, zstd",
            "-H", "x-client-bundle-id: com.deepseek.chat",
            "-H", "x-client-platform: web",
            "-H", "x-client-version: 1.0.0",
            "-H", "x-client-locale: en_US",
            "-H", "x-client-timezone-offset: 28800",
            "-H", "Referer: https://platform.deepseek.com/usage",
            "-H", "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:153.0) Gecko/20100101 Firefox/153.0",
            "-H", "DNT: 1",
            "-H", "Sec-GPC: 1",
            "-H", "Connection: keep-alive",
            "--compressed",
            "--max-time", str(timeout),
        ]

        # Add Bearer token header if available
        if self.bearer_token:
            headers.extend(["-H", f"authorization: Bearer {self.bearer_token}"])

        cmd = ["curl", "-s", "-L", "--http1.1", *headers, url]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            raise Exception(f"curl request failed: {result.stderr}")

        text = result.stdout.strip()
        if text.startswith(("<!DOCTYPE", "<!doctype", "<html", "<HTML")):
            title_match = re.search(r"<title>(.*?)</title>", text, re.IGNORECASE)
            title = title_match.group(1) if title_match else "Unknown"
            if "login" in text.lower() or "sign in" in text.lower():
                raise Exception(f"Login required: {title}")
            elif "challenge" in text.lower():
                raise Exception(f"WAF challenge: {title}")
            else:
                raise Exception(f"API returned HTML: {title}")

        return json.loads(text)

    # Token types that count toward token usage (REQUEST is a count, not tokens)
    TOKEN_TYPES = {"PROMPT_TOKEN", "PROMPT_CACHE_HIT_TOKEN",
                   "PROMPT_CACHE_MISS_TOKEN", "RESPONSE_TOKEN"}

    def _parse_token_amount(self, data: dict) -> dict[str, Any]:
        """Parse /api/v0/usage/amount response.

        Response shape:
          {"code":0, "data":{"biz_data":{"total":[{"model":"...","usage":[
            {"type":"PROMPT_TOKEN","amount":"0"}, ...]}]}}}
        """
        result: dict[str, Any] = {"models": []}
        try:
            biz_data = data.get("data", {}).get("biz_data", {})
            if isinstance(biz_data, dict):
                models = biz_data.get("total", [])
            else:
                models = []
        except (AttributeError, TypeError):
            return result

        total_tokens = 0.0
        for model_entry in models:
            model_name = model_entry.get("model", "unknown")
            model_tokens = 0.0
            for usage in model_entry.get("usage", []):
                typ = usage.get("type", "")
                amt = float(usage.get("amount", 0))
                if typ in self.TOKEN_TYPES:
                    model_tokens += amt
                    total_tokens += amt
            result["models"].append({
                "model": model_name,
                "tokens": model_tokens,
            })

        result["total_tokens"] = total_tokens
        return result

    def _parse_cost(self, data: dict) -> dict[str, Any]:
        """Parse /api/v0/usage/cost response.

        Response shape:
          {"code":0, "data":{"biz_data":[{"total":[{"model":"...","usage":[
            {"type":"PROMPT_TOKEN","amount":"6.25"}, ...]}]}]}}
        """
        result: dict[str, Any] = {"models": []}
        try:
            biz_data = data.get("data", {}).get("biz_data", [])
            if not isinstance(biz_data, list):
                biz_data = [biz_data]
        except (AttributeError, TypeError):
            biz_data = []

        total_cost = 0.0
        for section in biz_data:
            if not isinstance(section, dict):
                continue
            for model_entry in section.get("total", []):
                model_name = model_entry.get("model", "unknown")
                model_cost = 0.0
                for usage in model_entry.get("usage", []):
                    amt = float(usage.get("amount", 0))
                    model_cost += amt
                    total_cost += amt
                result["models"].append({
                    "model": model_name,
                    "cost": model_cost,
                })

        result["total_cost"] = total_cost
        result["peak_valley"] = self._peak_valley_status()
        return result

    def fetch_quota(self) -> QuotaInfo:
        """Fetch DeepSeek quota from usage endpoints."""
        quota_info = QuotaInfo(
            provider=self.name,
            plan_type="API Billing",
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        if not self.bearer_token:
            quota_info.error = (
                "No Bearer token found. Log in to platform.deepseek.com "
                "in Firefox, or set QUOTA_DEEPSEEK_TOKEN env var."
            )
            quota_info.error_type = "client"
            return quota_info

        year, month = self._get_local_year_month()

        # Fetch token amount
        amount_raw: dict[str, Any] = {}
        cost_raw: dict[str, Any] = {}
        amount_parsed: dict[str, Any] = {}
        cost_parsed: dict[str, Any] = {}

        try:
            amount_raw = self._curl_request(
                f"{self.AMOUNT_URL}?month={month}&year={year}"
            )
            amount_parsed = self._parse_token_amount(amount_raw)
        except Exception as e:
            quota_info.error = f"amount endpoint: {e}"
            quota_info.error_type = "client"
            # Still report what we have
            quota_info.raw_response = {
                "amount_raw": amount_raw,
                "cost_raw": cost_raw,
            }
            quota_info.sessions["monthly"] = SessionQuota(
                extra={"note": str(e)},
            )
            return quota_info

        try:
            cost_raw = self._curl_request(
                f"{self.COST_URL}?month={month}&year={year}"
            )
            cost_parsed = self._parse_cost(cost_raw)
        except Exception as e:
            # Cost is secondary; don't fail entirely if amount succeeded
            cost_parsed = {"error": str(e)}

        quota_info.raw_response = {
            "amount_raw": amount_raw,
            "cost_raw": cost_raw,
            "amount_parsed": amount_parsed,
            "cost_parsed": cost_parsed,
            "month": month,
            "year": year,
        }

        # Build session data: separate tokens and cost
        total_tokens = amount_parsed.get("total_tokens")
        total_cost = cost_parsed.get("total_cost")
        currency = cost_parsed.get("currency") or "CNY"
        peak_valley = cost_parsed.get("peak_valley") or self._peak_valley_status()

        # Token usage session (counts, no known limit)
        if total_tokens is not None:
            quota_info.sessions["monthly_tokens"] = SessionQuota(
                used=total_tokens,
                currency=None,
                extra={
                    "month": month,
                    "year": year,
                    "peak_valley_label": peak_valley.get("label", "?"),
                    "peak_valley_is_peak": peak_valley.get("is_peak", False),
                    "peak_valley_now_utc": peak_valley.get("now_utc_str", ""),
                    "peak_valley_windows": peak_valley.get("active_windows", []),
                    "models": amount_parsed.get("models", []),
                },
            )

        # Cost session (currency amount)
        if total_cost is not None:
            quota_info.sessions["monthly_cost"] = SessionQuota(
                used=total_cost,
                currency=currency,
                extra={
                    "month": month,
                    "year": year,
                    "peak_valley_label": peak_valley.get("label", "?"),
                    "peak_valley_is_peak": peak_valley.get("is_peak", False),
                    "peak_valley_now_utc": peak_valley.get("now_utc_str", ""),
                    "peak_valley_windows": peak_valley.get("active_windows", []),
                    "peak_valley_note": peak_valley.get("note", ""),
                    "models": cost_parsed.get("models", []),
                },
            )

        # If neither token nor cost parsed successfully, note it
        if total_tokens is None and total_cost is None:
            quota_info.sessions["monthly"] = SessionQuota(
                extra={
                    "month": month,
                    "year": year,
                    "note": "Unable to parse response — check raw_response",
                },
            )

        # If cost fetch failed, note it in the session
        if "error" in cost_parsed:
            cost_error = cost_parsed["error"]
            if quota_info.error:
                quota_info.error += f"; cost: {cost_error}"
            else:
                quota_info.error = f"cost endpoint: {cost_error}"
                quota_info.error_type = "client"

        return quota_info


# ============================================================================
# Provider Registry
# ============================================================================


class ProviderRegistry:
    """Registry for quota providers"""

    _providers: dict[str, type[QuotaProvider]] = {}

    @classmethod
    def register(cls, provider_class: type[QuotaProvider]) -> type[QuotaProvider]:
        """Register a provider class"""
        cls._providers[provider_class.name] = provider_class
        return provider_class

    # Providers that resolve their own credentials (no Firefox cookies)
    TOKEN_BASED = {"codex", "deepseek"}

    @classmethod
    def get(cls, name: str, cookie_header: str,
            token_manager: Optional["TokenManager"] = None) -> Optional[QuotaProvider]:
        """Get a provider instance by name.

        token_manager, if provided, enables OAuth token-based auth for
        supported providers (Claude, Codex) with auto-refresh capability.
        """
        provider_class = cls._providers.get(name)
        if provider_class:
            if name == "codex":
                try:
                    return CodexQuotaProvider.from_codex_config(
                        token_manager=token_manager
                    )
                except Exception:
                    return None
            # Claude gets token_manager injected; it decides OAuth vs cookie
            if name == "claude":
                return provider_class(cookie_header, token_manager=token_manager)
            return provider_class(cookie_header)
        return None

    @classmethod
    def list_providers(cls) -> list[tuple[str, str]]:
        """List all registered providers"""
        return [(name, cls._providers[name].description) for name in cls._providers]

    @classmethod
    def get_names(cls) -> list[str]:
        """Get all provider names"""
        return list(cls._providers.keys())


# Register providers
ProviderRegistry.register(ClaudeQuotaProvider)
ProviderRegistry.register(MiniMaxQuotaProvider)
ProviderRegistry.register(GLMQuotaProvider)
ProviderRegistry.register(KimiQuotaProvider)
ProviderRegistry.register(CodexQuotaProvider)
ProviderRegistry.register(DoubaoQuotaProvider)
ProviderRegistry.register(DeepSeekQuotaProvider)


# ============================================================================
# Cookie Manager - Firefox Cookie Extraction
# ============================================================================


class CookieManager:
    """Manages cookie extraction from Firefox"""

    DEFAULT_CACHE_DIR = Path("/tmp/quota_cache")
    COOKIE_CACHE_TTL = 86400  # 24 hours

    # Only cache the essential cookie per provider (minimise exposure)
    ESSENTIAL_COOKIES: dict[str, list[str]] = {
        "claude": ["sessionKey"],
        "minimax": ["HERTZ-SESSION"],
        "glm": ["bigmodel_token_production"],
        "kimi": ["kimi-auth"],
        "doubao": ["digest", "connect.sid"],
        "deepseek": ["HWWAFSESID"],
    }

    def __init__(self, profile: Optional[str] = None, cache_dir: Optional[Path] = None):
        self.profile = profile
        self.cookie_cache_dir = cache_dir or self.DEFAULT_CACHE_DIR

    def _ensure_cache_dir(self) -> None:
        """Create cookie cache directory with user-only permissions (0o700)."""
        if not self.cookie_cache_dir.exists():
            self.cookie_cache_dir.mkdir(parents=True, exist_ok=True)
            os.chmod(str(self.cookie_cache_dir), 0o700)
        else:
            # Fix permissions if directory already exists (e.g., created by DebounceManager)
            try:
                st = self.cookie_cache_dir.stat()
                if st.st_mode & 0o077:  # group/other bits set
                    os.chmod(str(self.cookie_cache_dir), 0o700)
            except OSError:
                pass

    def _cookie_cache_path(self, provider_name: str) -> Path:
        return self.cookie_cache_dir / f"cookies_{provider_name}.json"

    def _load_cached_cookies(self, provider_name: str) -> Optional[dict[str, str]]:
        """Load cached cookies for provider if fresh enough."""
        path = self._cookie_cache_path(provider_name)
        try:
            if not path.exists():
                return None
            data = json.loads(path.read_text())
            cached_at = data.get("_time", 0)
            if time.time() - cached_at > self.COOKIE_CACHE_TTL:
                return None
            cookies = data.get("cookies")
            if cookies and isinstance(cookies, dict):
                return cookies
        except (json.JSONDecodeError, IOError, KeyError):
            pass
        return None

    def _save_cached_cookies(self, provider_name: str, cookies: dict[str, str]) -> None:
        """Save essential cookies for provider with user-only permissions (0o600)."""
        self._ensure_cache_dir()
        essential_keys = self.ESSENTIAL_COOKIES.get(provider_name, [])
        # Only persist the essential cookies
        filtered = {k: v for k, v in cookies.items() if k in essential_keys}
        if not filtered:
            return

        path = self._cookie_cache_path(provider_name)
        tmp_path = path.with_suffix(".tmp")
        try:
            tmp_path.write_text(json.dumps(
                {"_time": time.time(), "cookies": filtered},
                ensure_ascii=False,
            ))
            os.chmod(str(tmp_path), 0o600)
            os.rename(str(tmp_path), str(path))
        except IOError:
            try:
                tmp_path.unlink(missing_ok=True)
            except OSError:
                pass

    def get_profiles_dir(self) -> Path:
        """Get Firefox profiles directory"""
        import platform

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
            raise Exception(f"Unsupported OS: {system}")

    def find_profiles(self) -> list[Path]:
        """Find all Firefox profiles"""
        profiles_dir = self.get_profiles_dir()
        if not profiles_dir.exists():
            raise Exception(f"Firefox profiles directory not found: {profiles_dir}")

        profiles = [p for p in profiles_dir.iterdir() if p.is_dir()]
        profiles.sort(key=lambda p: p.stat().st_mtime, reverse=True)
        return profiles

    def extract_cookies(self, cookies_db: Path, host_pattern: str) -> dict[str, str]:
        """Extract cookies matching host pattern from Firefox cookies.sqlite"""
        cookies = {}
        if not cookies_db.exists():
            return cookies

        import tempfile
        import shutil

        with tempfile.NamedTemporaryFile(suffix=".sqlite", delete=False) as tmp:
            tmp_path = Path(tmp.name)

        try:
            shutil.copy2(cookies_db, tmp_path)
            conn = sqlite3.connect(tmp_path)
            cursor = conn.cursor()

            cursor.execute(
                """
                SELECT name, value, host
                FROM moz_cookies
                WHERE host = ? OR host LIKE ?
                ORDER BY expiry DESC
                """,
                (host_pattern, f"%.{host_pattern}"),
            )

            for name, value, host in cursor.fetchall():
                if name not in cookies:
                    cookies[name] = value

            conn.close()
        finally:
            try:
                tmp_path.unlink()
            except:
                pass

        return cookies

    def build_cookie_header(self, cookies: dict[str, str]) -> str:
        """Build Cookie header from cookies dict"""
        return "; ".join(f"{k}={v}" for k, v in cookies.items())

    def get_cookies_for_provider(
        self, provider_name: str, profile: Optional[str] = None
    ) -> tuple[str, str]:
        """
        Get cookies for a specific provider.

        Returns:
            tuple: (cookie_header, source_description)
        """
        profile = profile or self.profile or os.environ.get("QUOTA_PROFILE")

        # Provider-specific cookie requirements
        # Codex uses auth token from ~/.codex/auth.json, not cookies
        host_patterns = {
            "claude": "claude.ai",
            "minimax": "minimaxi.com",
            "glm": "bigmodel.cn",
            "kimi": "kimi.com",
            "doubao": "volcengine.com",
            "deepseek": "platform.deepseek.com",
        }

        host_pattern = host_patterns.get(provider_name)
        if not host_pattern:
            raise Exception(f"Unknown provider: {provider_name}")

        # Try cookie cache first (avoids hitting Firefox SQLite)
        cached_cookies = self._load_cached_cookies(provider_name)
        if cached_cookies:
            essential = self.ESSENTIAL_COOKIES.get(provider_name, [])
            if all(k in cached_cookies for k in essential):
                header = self.build_cookie_header(cached_cookies)
                return header, "cookie cache"

        profiles_dir = self.get_profiles_dir()

        if profile:
            profile_path = profiles_dir / profile
            if not profile_path.exists():
                raise Exception(f"Profile not found: {profile_path}")
            profiles = [profile_path]
        else:
            profiles = self.find_profiles()

        for prof in profiles:
            cookies_db = prof / "cookies.sqlite"
            cookies = self.extract_cookies(cookies_db, host_pattern)

            # Provider-specific validation - just check if we have cookies
            if cookies:
                # Check provider-specific essential cookies
                if provider_name == "claude" and "sessionKey" not in cookies:
                    continue
                if provider_name == "minimax" and "HERTZ-SESSION" not in cookies:
                    continue
                if provider_name == "glm" and "bigmodel_token_production" not in cookies:
                    continue
                if provider_name == "kimi" and "kimi-auth" not in cookies:
                    continue
                if provider_name == "doubao" and "digest" not in cookies:
                    continue
                if provider_name == "deepseek" and "HWWAFSESID" not in cookies:
                    continue
                # Codex uses auth token from ~/.codex/auth.json, verified separately

                # Cache the essential cookies for next time
                self._save_cached_cookies(provider_name, cookies)

                header = self.build_cookie_header(cookies)
                return header, f"Firefox profile: {prof.name}"

        raise Exception(f"No valid credentials found for {provider_name}")


# ============================================================================
# Token Manager - OAuth Token Storage & Refresh
# ============================================================================


class TokenManager:
    """Manages OAuth tokens for providers that support token-based auth.

    Tokens are persisted to /tmp/quota_cache/tokens.json (0o600).
    Supports automatic refresh via OAuth refresh_token grant for
    Claude and Codex providers.

    Public OAuth client_ids (community-known, same as the iOS widget uses):
      Claude:  9d1c250a-e61b-44d9-88ed-5944d1962f5e
      Codex:   app_EMoamEEZ73f0CkXaXp7hrann
    """

    TOKEN_FILE = Path("/tmp/quota_cache/tokens.json")

    OAUTH_CONFIG = {
        "claude": {
            "refresh_url": "https://console.anthropic.com/v1/oauth/token",
            "client_id": "9d1c250a-e61b-44d9-88ed-5944d1962f5e",
        },
        "codex": {
            "refresh_url": "https://auth.openai.com/oauth/token",
            "client_id": "app_EMoamEEZ73f0CkXaXp7hrann",
        },
    }

    def __init__(self, token_file: Optional[Path] = None):
        self._token_file = token_file or self.TOKEN_FILE
        self._tokens: dict[str, dict[str, Any]] = {}
        self._loaded = False

    def _ensure_loaded(self) -> None:
        if self._loaded:
            return
        self._loaded = True
        try:
            if self._token_file.exists():
                self._tokens = json.loads(self._token_file.read_text())
        except (json.JSONDecodeError, IOError):
            self._tokens = {}

    def _save(self) -> None:
        """Atomically write tokens with restricted permissions."""
        self._token_file.parent.mkdir(parents=True, exist_ok=True)
        tmp_path = self._token_file.with_suffix(".tmp")
        try:
            tmp_path.write_text(json.dumps(self._tokens, ensure_ascii=False))
            os.chmod(str(tmp_path), 0o600)
            os.rename(str(tmp_path), str(self._token_file))
        except IOError:
            try:
                tmp_path.unlink(missing_ok=True)
            except OSError:
                pass

    def has_tokens(self, provider: str) -> bool:
        """Check if we have tokens for a provider."""
        self._ensure_loaded()
        tok = self._tokens.get(provider)
        return bool(tok and tok.get("access_token"))

    def get_valid_token(self, provider: str) -> Optional[str]:
        """Get a valid access token, auto-refreshing if near expiry.

        Refreshes proactively when within 60 seconds of expiry.
        Returns None if no token is available.
        """
        self._ensure_loaded()
        tok = self._tokens.get(provider)
        if not tok or not tok.get("access_token"):
            return None

        expires_at = tok.get("expires_at", 0)
        if expires_at and time.time() > expires_at - 60:
            try:
                self._refresh(provider)
                tok = self._tokens.get(provider, {})
            except Exception:
                pass  # Return current token; caller handles 401 if expired

        return tok.get("access_token")

    def force_refresh(self, provider: str) -> Optional[str]:
        """Force a token refresh. Returns new access_token or None."""
        self._ensure_loaded()
        tok = self._tokens.get(provider)
        if not tok or not tok.get("refresh_token"):
            return None
        try:
            self._refresh(provider)
            return self._tokens.get(provider, {}).get("access_token")
        except Exception:
            return None

    def _refresh(self, provider: str) -> None:
        """Execute OAuth refresh_token grant for a provider."""
        config = self.OAUTH_CONFIG.get(provider)
        if not config:
            raise Exception(f"No OAuth config for provider: {provider}")

        tok = self._tokens.get(provider, {})
        refresh_token = tok.get("refresh_token")
        if not refresh_token:
            raise Exception(f"No refresh_token for {provider}")

        cmd = [
            "curl", "-s", "-L", "--http1.1",
            "-X", "POST",
            "-H", "Content-Type: application/json",
            "--max-time", "10",
            "-d", json.dumps({
                "grant_type": "refresh_token",
                "refresh_token": refresh_token,
                "client_id": config["client_id"],
            }),
            config["refresh_url"],
        ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            raise Exception(f"Token refresh failed: {result.stderr}")

        data = json.loads(result.stdout)
        if not data.get("access_token"):
            detail = result.stdout[:200]
            raise Exception(f"Token refresh returned no access_token: {detail}")

        new_token = {
            "access_token": data["access_token"],
            "refresh_token": data.get("refresh_token", refresh_token),
            "expires_at": time.time() + data.get("expires_in", 3600),
        }
        # Preserve account_id for codex
        if "account_id" in tok:
            new_token["account_id"] = tok["account_id"]
        self._tokens[provider] = new_token
        self._save()

    def import_tokens(self, data: dict[str, Any]) -> int:
        """Import tokens from a dict.

        Expected format: {"claude": {"accessToken": "...", "refreshToken": "...", ...}, ...}
        Supports both camelCase (from JS widget) and snake_case keys.
        Returns count of imported providers.
        """
        self._ensure_loaded()
        count = 0
        for provider, tok in data.items():
            if not isinstance(tok, dict):
                continue
            access_token = tok.get("access_token") or tok.get("accessToken")
            refresh_token = tok.get("refresh_token") or tok.get("refreshToken")
            if not access_token:
                continue

            expires_at = tok.get("expires_at") or tok.get("expiresAt")
            if isinstance(expires_at, (int, float)) and expires_at < 10000000000:
                # Looks like seconds since epoch; ensure it's not in ms
                pass
            elif isinstance(expires_at, (int, float)):
                expires_at = expires_at  # already correct

            imported = {
                "access_token": access_token,
                "refresh_token": refresh_token or "",
                "expires_at": expires_at or (time.time() + 3600),
            }
            account_id = tok.get("account_id") or tok.get("accountId")
            if account_id:
                imported["account_id"] = account_id

            self._tokens[provider] = imported
            count += 1

        if count > 0:
            self._save()
        return count

    def get_account_id(self, provider: str) -> Optional[str]:
        """Get account_id for a provider (needed by Codex)."""
        self._ensure_loaded()
        tok = self._tokens.get(provider)
        if tok:
            return tok.get("account_id")
        return None

    def clear(self, provider: Optional[str] = None) -> None:
        """Clear tokens for a provider, or all if provider is None."""
        self._ensure_loaded()
        if provider:
            self._tokens.pop(provider, None)
        else:
            self._tokens.clear()
        self._save()


# ============================================================================
# Output Formatters
# ============================================================================


def format_duration(total_seconds: int, target_dt: Optional[datetime] = None) -> str:
    """Format seconds into human-readable duration (e.g., '2d 3h', '5h 30m', '45m').
    
    For durations >= 1 day, includes the target Month-day if target_dt is provided.
    Format: 'Mar 15 in 2d 3h' or just '2d 3h' if no target_dt.
    """
    if total_seconds <= 0:
        return "now"

    days = total_seconds // 86400
    hours = (total_seconds % 86400) // 3600
    minutes = (total_seconds % 3600) // 60

    if days > 0:
        duration = f"{days}d {hours}h"
        if target_dt is not None:
            # Include Month-day for weekly/monthly windows
            date_str = target_dt.strftime("%b %d").replace(" 0", " ")
            return f"{date_str}, {duration}"
        return duration
    elif hours > 0:
        return f"{hours}h {minutes}m"
    else:
        return f"{minutes}m"


def format_token_count(count: float) -> str:
    """Format token count with K/M suffixes for readability.
    
    >= 1M:  1.23M, 12.35M
    >= 10K: 12.3K, 500K
    >= 1K:  1.23K, 10K
    < 1K:   raw integer (500, 999)
    
    Trailing .0 / .00 is stripped for cleaner display.
    """
    if count >= 1_000_000:
        scaled = count / 1_000_000
        formatted = f"{scaled:.2f}".rstrip("0").rstrip(".")
        return f"{formatted}M"
    elif count >= 10_000:
        scaled = count / 1_000
        formatted = f"{scaled:.1f}".rstrip("0").rstrip(".")
        return f"{formatted}K"
    elif count >= 1_000:
        scaled = count / 1_000
        formatted = f"{scaled:.2f}".rstrip("0").rstrip(".")
        return f"{formatted}K"
    else:
        return f"{count:.0f}"


def format_percent(percent: Optional[float], no_color: bool = False) -> str:
    """Format percentage with optional color"""
    if percent is None:
        return "N/A"
    if no_color or os.environ.get("NO_COLOR") or not sys.stdout.isatty():
        return f"{percent:.1f}%"
    color = "\033[32m" if percent < 50 else "\033[33m" if percent < 80 else "\033[31m"
    reset = "\033[0m"
    return f"{color}{percent:.1f}%{reset}"


def format_session_label(label: str) -> str:
    """Format session label for display"""
    labels = {
        "current": "5-hour Session",
        "weekly": "Weekly",
        "weekly_opus": "Opus Model (Weekly)",
        "weekly_sonnet": "Sonnet Model (Weekly)",
        "extra_credits": "Extra Credits",
        "daily": "Daily",
        "quota": "Prompt Quota",
        "mcp_monthly": "MCP Monthly Quota",
        "code_review": "Code Review",
        "monthly": "Monthly",
        "monthly_tokens": "Monthly Tokens",
        "monthly_cost": "Monthly Cost",
    }
    # Check for exact match first (like mcp_monthly)
    if label in labels:
        return labels[label]
    # Handle MCP service sessions (mcp_search-prime, etc.)
    if label.startswith("mcp_"):
        model_code = label[4:]  # Remove "mcp_" prefix
        return f"MCP: {model_code}"
    return labels.get(label, label.replace("_", " ").title())


def format_reset_date(resets_at: Optional[str], resets_at_local: Optional[str], resets_in: Optional[str]) -> str:
    """Format reset time as readable date (e.g., 'Mar 5 @ 01:37 (2h 30m)')"""
    if not resets_at:
        if resets_in:
            return f"Resets: {resets_in}"
        return ""
    
    try:
        dt = datetime.fromisoformat(resets_at.replace("Z", "+00:00"))
        local_dt = dt.astimezone(get_local_timezone())
        date_str = local_dt.strftime("%b %d").replace(" 0", " ")
        time_str = resets_at_local or local_dt.strftime("%H:%M")
        
        result = f"Resets: {date_str} @ {time_str}"
        if resets_in:
            result += f" ({resets_in})"
        return result
    except Exception:
        if resets_at_local and resets_in:
            return f"Resets: {resets_at_local} ({resets_in})"
        elif resets_in:
            return f"Resets: {resets_in}"
        return ""


def format_expiry_date(expires_at: Optional[str], expires_at_local: Optional[str], expires_in: Optional[str]) -> str:
    """Format expiry time as readable date (e.g., 'Mar 5 @ 01:37 (2h 30m)')"""
    if not expires_at:
        return expires_in or ""

    try:
        dt = datetime.fromisoformat(expires_at.replace("Z", "+00:00"))
        local_dt = dt.astimezone(get_local_timezone())
        date_str = local_dt.strftime("%b %d").replace(" 0", " ")
        time_str = expires_at_local or local_dt.strftime("%H:%M")

        result = f"{date_str} @ {time_str}"
        if expires_in:
            result += f" ({expires_in})"
        return result
    except (TypeError, ValueError):
        if expires_at_local and expires_in:
            return f"{expires_at_local} ({expires_in})"
        if expires_in:
            return expires_in
        return expires_at


def format_reset_credits(quota_info: QuotaInfo) -> list[str]:
    """Format provider-level reset credits for terminal display."""
    has_reset_data = (
        quota_info.reset_credit_count is not None
        or bool(quota_info.reset_credits)
        or bool(quota_info.reset_credits_error)
    )
    if not has_reset_data:
        return []

    lines = ["\n  Reset Credits"]

    if quota_info.reset_credit_count is not None:
        label = "reset" if quota_info.reset_credit_count == 1 else "resets"
        lines.append(f"   Available: {quota_info.reset_credit_count} {label}")

    if quota_info.reset_credits_error:
        lines.append(f"   Details: unavailable ({quota_info.reset_credits_error})")

    credits = [credit for credit in quota_info.reset_credits if credit.status == "available"]
    if not credits:
        credits = quota_info.reset_credits

    if credits:
        lines.append("   Expires:")
        for credit in credits:
            expiry = format_expiry_date(credit.expires_at, credit.expires_at_local, credit.expires_in)
            details = []
            if credit.status:
                details.append(credit.status)
            if credit.reset_type and credit.reset_type != "codex_rate_limits":
                details.append(credit.reset_type)

            suffix = f" [{', '.join(details)}]" if details else ""
            lines.append(f"    - {expiry or 'expires unknown'}{suffix}")

    return lines


def format_rich(quota_info: QuotaInfo, no_color: bool = False) -> str:
    """Format quota info for rich terminal display"""
    lines = []
    lines.append("")
    lines.append("=" * 55)
    plan_label = quota_info.plan_type or "Coding Plan Quota"
    lines.append(f"  {quota_info.provider.upper()} {plan_label}")
    lines.append("=" * 55)

    # Check for error - either at top level or nested in raw_response
    error_msg = quota_info.error
    if not error_msg and quota_info.raw_response:
        raw_error = quota_info.raw_response.get("error")
        if raw_error:
            error_msg = raw_error.get("message") if isinstance(raw_error, dict) else str(raw_error)

    if error_msg:
        lines.append(f"\n  Error: {error_msg}")

        # If there's cached session data, show it too
        if quota_info.sessions:
            lines.append(f"\n  (Showing cached data)")
            # Continue to show cached sessions below
        else:
            lines.append("")
            lines.append("=" * 55)
            return "\n".join(lines)

    for session_key, session in quota_info.sessions.items():
        label = format_session_label(session_key)
        lines.append(f"\n  {label}")

        if session.is_enabled is False:
            lines.append("   Status: Disabled")
            continue

        # Check if this is an MCP session (only has used count, no percentage)
        is_mcp = session.extra.get("is_mcp", False) if session.extra else False

        if is_mcp:
            # MCP sessions only show usage count
            if session.used is not None:
                lines.append(f"   Prompts: {session.used:.0f}")
            else:
                lines.append("   Status: No data")
            continue

        # If we have prompt counts, show Usage only (skip percentages)
        # If we only have percentages, show those
        has_prompt_counts = session.used is not None and session.limit is not None
        has_usage_only = session.used is not None and session.limit is None

        if has_prompt_counts:
            # Show prompt usage
            if session.currency:
                # Currency-based usage (like Claude extra credits)
                lines.append(f"   Usage: {session.currency} ${session.used:.2f} / ${session.limit:.2f}")
            else:
                # Count-based usage (like prompt count)
                used_str = format_token_count(session.used)
                limit_str = format_token_count(session.limit)
                pct_str = f" ({session.used_percent:.1f}%)" if session.used_percent is not None else ""
                lines.append(f"   Prompts: {used_str} / {limit_str}{pct_str}")
        elif has_usage_only:
            # Usage count without a known limit (e.g., DeepSeek monthly tokens/cost)
            if session.currency:
                lines.append(f"   Cost: {session.currency} {session.used:.4f}")
            else:
                lines.append(f"   Tokens: {format_token_count(session.used)}")
        elif session.used_percent is not None:
            # Only percentages available (no prompt counts) - just show used
            lines.append(f"   Used: {format_percent(session.used_percent, no_color)}")

        # Reset time
        reset_str = format_reset_date(session.resets_at, session.resets_at_local, session.resets_in)
        if reset_str:
            lines.append(f"   {reset_str}")

        # Show MCP usage details for mcp_monthly session
        if session_key == "mcp_monthly" and session.extra and "usage_details" in session.extra:
            usage_details = session.extra["usage_details"]
            if usage_details:
                mcp_items = []
                for detail in usage_details:
                    model_code = detail.get("modelCode", "unknown")
                    usage = detail.get("usage", 0)
                    mcp_items.append(f"{model_code}: {usage}")
                lines.append(f"   Services: {', '.join(mcp_items)}")

        # Show peak-valley pricing status for DeepSeek sessions
        if session.extra and "peak_valley_label" in session.extra:
            pv_label = session.extra["peak_valley_label"]
            pv_is_peak = session.extra.get("peak_valley_is_peak", False)
            pv_now = session.extra.get("peak_valley_now_utc", "")
            pv_windows = session.extra.get("peak_valley_windows", [])
            if pv_label:
                if pv_is_peak:
                    color_on = "\033[31m" if not (no_color or os.environ.get("NO_COLOR") or not sys.stdout.isatty()) else ""
                    color_off = "\033[0m" if color_on else ""
                    window_str = ", ".join(pv_windows) if pv_windows else ""
                    lines.append(f"   Pricing: {color_on}{pv_label}{color_off} (now {pv_now} UTC, in {window_str})")
                else:
                    lines.append(f"   Pricing: {pv_label}")

        if not has_prompt_counts and not has_usage_only and session.used_percent is None and not reset_str:
            lines.append("   Status: No data")

    lines.extend(format_reset_credits(quota_info))

    lines.append("")
    lines.append("=" * 55)
    return "\n".join(lines)


def format_json(quota_info: QuotaInfo, pretty: bool = True) -> str:
    """Format quota info as JSON"""
    data = quota_info.to_dict()
    if pretty:
        return json.dumps(data, indent=2, ensure_ascii=False)
    return json.dumps(data, ensure_ascii=False)


def format_all_rich(quotas: list[QuotaInfo], no_color: bool = False) -> str:
    """Format multiple quota infos for display"""
    return "\n".join(format_rich(q, no_color) for q in quotas)


def format_all_json(quotas: list[QuotaInfo], pretty: bool = True) -> str:
    """Format multiple quota infos as JSON array"""
    data = [q.to_dict() for q in quotas]
    if pretty:
        return json.dumps(data, indent=2, ensure_ascii=False)
    return json.dumps(data, ensure_ascii=False)


def is_cookie_error(error_msg: str) -> bool:
    """Check if error is due to invalid/expired cookies."""
    error_lower = error_msg.lower()
    cookie_errors = [
        "cookie is missing",
        "cookie expired",
        "login required",
        "not logged in",
        "authentication required",
        "unauthorized",
        "invalid cookie",
        "session expired",
        "empty response",
        "token may be expired",
    ]
    return any(phrase in error_lower for phrase in cookie_errors)


def clear_provider_cookie_cache(provider: str, cache_dir: Path) -> None:
    """Clear cookie cache for a specific provider."""
    cookie_cache_path = cache_dir / f"cookies_{provider}.json"
    try:
        cookie_cache_path.unlink(missing_ok=True)
    except IOError:
        pass


# ============================================================================
# Debounce Manager (for status bar usage)
# ============================================================================


class DebounceManager:
    """Manages debounce mechanism for API calls with per-provider cache files.

    Each provider gets its own cache file (e.g., claude.json, kimi.json) to
    eliminate read-modify-write races when multiple agents query different
    providers concurrently.  Writes are atomic (write .tmp then os.rename).

    Same-provider concurrency is handled via fcntl.flock:
    - First agent to acquire the lock performs the API fetch and writes cache.
    - Other agents fail to acquire the lock (non-blocking LOCK_EX|LOCK_NB),
      return stale cache immediately (stale-while-revalidate).
    - If no stale cache exists (cold start), they block briefly on the lock
      so they can read the freshly written cache.
    """

    DEFAULT_CACHE_DIR = Path("/tmp/quota_cache")
    DEFAULT_INTERVAL = 60
    LOCK_STALE_SECONDS = 300  # consider lock stale after 5 minutes

    def __init__(self, interval_seconds: int = DEFAULT_INTERVAL, cache_dir: Optional[Path] = None):
        self.interval = interval_seconds
        self.cache_dir = cache_dir or self.DEFAULT_CACHE_DIR
        if not self.cache_dir.exists():
            self.cache_dir.mkdir(parents=True, exist_ok=True)
            os.chmod(str(self.cache_dir), 0o700)

    def _provider_path(self, provider: str) -> Path:
        return self.cache_dir / f"{provider}.json"

    def _lock_path(self, provider: str) -> Path:
        return self.cache_dir / f"{provider}.lock"

    def _read_provider(self, provider: str) -> Optional[dict[str, Any]]:
        """Read a single provider cache file. Returns None on missing/corrupt."""
        path = self._provider_path(provider)
        try:
            if path.exists():
                return json.loads(path.read_text())
        except (json.JSONDecodeError, IOError):
            pass
        return None

    def _write_provider(self, provider: str, data: dict[str, Any]) -> None:
        """Atomically write provider cache: write .tmp then rename."""
        path = self._provider_path(provider)
        tmp_path = path.with_suffix(".tmp")
        tmp_path.write_text(json.dumps(data, ensure_ascii=False, indent=2))
        os.rename(str(tmp_path), str(path))

    def _is_cache_fresh(self, entry: Optional[dict[str, Any]]) -> bool:
        if entry is None:
            return False
        # Client errors (curl/network failures) are transient -- always retry.
        # Server errors (API error responses) are legitimate -- cache normally.
        # Legacy entries without error_type but with error are treated as client errors.
        data = entry.get("data", {})
        if isinstance(data, dict) and "error" in data:
            if data.get("error_type", "client") == "client":
                return False
        return time.time() - entry.get("_time", 0) < self.interval

    def _get_stale_data(self, provider: str) -> Optional[dict]:
        """Return cached data regardless of freshness (for stale-while-revalidate)."""
        entry = self._read_provider(provider)
        if entry is not None:
            return entry.get("data")
        return None

    def get_cached(self, provider: str) -> Optional[dict]:
        """Get cached result if still valid"""
        entry = self._read_provider(provider)
        if self._is_cache_fresh(entry):
            return entry.get("data")
        return None

    def set_cache(self, provider: str, data: dict) -> None:
        """Cache result for provider (atomic write)"""
        self._write_provider(provider, {"_time": time.time(), "data": data})

    def fetch_or_cached(
        self, provider: str, fetch_fn: "Callable[[], QuotaInfo]", force: bool = False
    ) -> QuotaInfo:
        """Fetch quota with lock-based deduplication across concurrent agents.

        - If cache is fresh and not forced, return cached data immediately.
        - If cache is stale, try to acquire a non-blocking lock:
          - Won lock: fetch, write cache, release lock, return fresh data.
          - Lost lock + have stale cache: return stale data (another agent is fetching).
          - Lost lock + no cache (cold start): block on lock, then read fresh cache.
        """
        import fcntl

        # Fast path: fresh cache
        if not force:
            entry = self._read_provider(provider)
            if self._is_cache_fresh(entry):
                return QuotaInfo.from_dict(entry["data"])

        lock_path = self._lock_path(provider)
        # Clean stale lock files (e.g., left by killed process)
        try:
            if lock_path.exists():
                lock_age = time.time() - lock_path.stat().st_mtime
                if lock_age > self.LOCK_STALE_SECONDS:
                    lock_path.unlink(missing_ok=True)
        except OSError:
            pass

        lock_fd = os.open(str(lock_path), os.O_CREAT | os.O_RDWR, 0o644)
        try:
            # Try non-blocking lock
            try:
                fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except (BlockingIOError, OSError):
                # Another agent holds the lock -- stale-while-revalidate
                stale = self._get_stale_data(provider)
                if stale is not None:
                    return QuotaInfo.from_dict(stale)
                # Cold start: no stale data, must wait for the other agent
                fcntl.flock(lock_fd, fcntl.LOCK_EX)  # blocking wait
                fresh = self._read_provider(provider)
                if fresh is not None and fresh.get("data"):
                    return QuotaInfo.from_dict(fresh["data"])
                # Fallback: other agent failed, we fetch ourselves below

            # Re-check cache under lock (another agent may have just finished)
            if not force:
                entry = self._read_provider(provider)
                if self._is_cache_fresh(entry):
                    return QuotaInfo.from_dict(entry["data"])

            # We hold the lock -- do the actual fetch
            # Touch lock file mtime so stale detection works
            os.utime(lock_fd)
            quota_info = fetch_fn()
            self.set_cache(provider, quota_info.to_dict())
            return quota_info
        finally:
            fcntl.flock(lock_fd, fcntl.LOCK_UN)
            os.close(lock_fd)

    def clear_cache(self, provider: Optional[str] = None) -> None:
        """Clear cache for provider or all (includes cookie cache)"""
        if provider:
            for prefix in (provider, f"cookies_{provider}"):
                for suffix in (".json", ".lock", ".tmp"):
                    path = self.cache_dir / f"{prefix}{suffix}"
                    try:
                        path.unlink(missing_ok=True)
                    except IOError:
                        pass
        else:
            for path in self.cache_dir.glob("*.json"):
                try:
                    path.unlink(missing_ok=True)
                except IOError:
                    pass
            for path in self.cache_dir.glob("*.lock"):
                try:
                    path.unlink(missing_ok=True)
                except IOError:
                    pass


# ============================================================================
# CLI Entry Point
# ============================================================================


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Multi-provider coding plan quota checker",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Query all providers
    python3 quota.py --all

    # Query specific provider
    python3 quota.py -p claude
    python3 quota.py -p minimax

    # JSON output for scripts
    python3 quota.py -p claude --json

    # Use specific Firefox profile
    QUOTA_PROFILE="xxx.default" python3 quota.py -p claude

    # Debounce mode (for status bars)
    python3 quota.py -p claude --debounce 60

Environment Variables:
    QUOTA_PROFILE     Firefox profile name to use for cookies
    QUOTA_TIMEZONE    IANA timezone name (e.g. "America/Vancouver")
    MINIMAX_API_KEY   MiniMax API key (if not using cookies)
    NO_COLOR          Disable colored output

OAuth Token Auth (Claude / Codex):
    Use --import-tokens to import OAuth tokens for refresh-based auth.
    Token file format: {"claude": {"access_token": "...", "refresh_token": "..."}, ...}
    Tokens are persisted to /tmp/quota_cache/tokens.json (0o600).
        """,
    )

    parser.add_argument(
        "-p", "--provider",
        metavar="NAME",
        help="Query specific provider (default: all if --all not specified)"
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Query all registered providers"
    )
    parser.add_argument(
        "--list-providers",
        action="store_true",
        help="List all registered providers and exit"
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Output as JSON (for scripts)"
    )
    parser.add_argument(
        "--no-color",
        action="store_true",
        help="Disable colored output"
    )
    parser.add_argument(
        "--profile",
        metavar="NAME",
        help="Firefox profile name (overrides QUOTA_PROFILE env)"
    )
    parser.add_argument(
        "--debounce",
        metavar="SECONDS",
        type=int,
        nargs="?",
        const=60,
        default=0,
        help="Debounce interval (for status bar usage)"
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force refresh, ignore debounce cache"
    )
    parser.add_argument(
        "--clear-cache",
        action="store_true",
        help="Clear debounce cache and exit"
    )
    parser.add_argument(
        "--import-tokens",
        metavar="FILE",
        help="Import OAuth tokens from JSON file (for Claude/Codex token-based auth)"
    )

    args = parser.parse_args()

    # Handle --list-providers
    if args.list_providers:
        print("Registered providers:")
        for name, desc in ProviderRegistry.list_providers():
            print(f"  {name:15} - {desc}")
        return 0

    # Handle --no-color
    if args.no_color:
        os.environ["NO_COLOR"] = "1"

    # Initialize token manager (for OAuth-based auth)
    token_manager = TokenManager()

    # Handle --import-tokens
    if args.import_tokens:
        try:
            data = json.loads(Path(args.import_tokens).read_text())
            count = token_manager.import_tokens(data)
            print(f"Imported tokens for {count} provider(s)", file=sys.stderr)
            return 0
        except (json.JSONDecodeError, IOError) as e:
            print(f"Failed to import tokens: {e}", file=sys.stderr)
            return 1

    # Initialize debounce manager and cookie manager (always needed for cache clearing)
    dm = DebounceManager(interval_seconds=args.debounce) if args.debounce > 0 else DebounceManager()
    cookie_manager = CookieManager(profile=args.profile)

    # Handle --clear-cache
    if args.clear_cache:
        # Clear both debounce cache and cookie cache
        dm.clear_cache(provider=args.provider)
        # Also clear the cookie cache file directly (cookies_<provider>.json)
        if args.provider:
            cookie_cache_path = dm.cache_dir / f"cookies_{args.provider}.json"
            try:
                cookie_cache_path.unlink(missing_ok=True)
            except IOError:
                pass
        print("Cache cleared", file=sys.stderr)
        return 0

    # Determine which providers to query
    if args.provider:
        providers_to_query = [args.provider]
    elif args.all:
        providers_to_query = ProviderRegistry.get_names()
    else:
        # Default: show help if no provider specified
        parser.print_help()
        return 1

    cookie_manager = CookieManager(profile=args.profile)

    # Pre-load cookies for cookie-based providers (skip token-based ones)
    TOKEN_BASED_PROVIDERS = ProviderRegistry.TOKEN_BASED
    TOKEN_CAPABLE = {"claude"}  # Providers that support OAuth tokens + cookie fallback
    cookies_cache: dict[str, tuple[str, str]] = {}
    try:
        for provider_name in providers_to_query:
            if provider_name in TOKEN_BASED_PROVIDERS:
                cookies_cache[provider_name] = (None, "token-based")
                continue
            try:
                cookie_header, source = cookie_manager.get_cookies_for_provider(provider_name)
                cookies_cache[provider_name] = (cookie_header, source)
            except Exception as e:
                cookies_cache[provider_name] = (None, str(e))
    except Exception:
        pass  # Will be handled per-provider below

    # Print credentials info once (exclude token-based providers)
    sources = [src for _, src in cookies_cache.values() if src and not src.startswith("Error") and src != "token-based"]
    if not sources and token_manager.has_tokens("claude"):
        sources = ["OAuth token"]
    if sources and not args.json:
        print(f"Credentials: {sources[0]}", file=sys.stderr)

    results: list[QuotaInfo] = []

    for provider_name in providers_to_query:
        if dm:
            # Debounce mode: use lock-based fetch_or_cached to deduplicate
            # concurrent API calls from multiple agents
            def make_fetcher(pname: str) -> Callable[[], QuotaInfo]:
                def _fetch() -> QuotaInfo:
                    # Codex uses its own auth from ~/.codex/
                    if pname in TOKEN_BASED_PROVIDERS:
                        try:
                            provider = ProviderRegistry.get(
                                pname, "", token_manager=token_manager
                            )
                            if not provider:
                                return QuotaInfo(provider=pname, error="Failed to resolve credentials")
                            return provider.fetch_quota()
                        except Exception as e:
                            return QuotaInfo(provider=pname, error=str(e))
                    cookie_header, _ = cookies_cache.get(pname, (None, ""))
                    if not cookie_header:
                        if pname in TOKEN_CAPABLE and token_manager.has_tokens(pname):
                            cookie_header = ""  # OAuth will be tried by the provider
                        else:
                            return QuotaInfo(provider=pname, error="No credentials found")
                    provider = ProviderRegistry.get(
                        pname, cookie_header, token_manager=token_manager
                    )
                    if not provider:
                        return QuotaInfo(provider=pname, error="Unknown provider")
                    result = provider.fetch_quota()
                    # Auto-retry once on cookie/auth errors with fresh cookies
                    if result.error and is_cookie_error(result.error):
                        clear_provider_cookie_cache(pname, dm.cache_dir)
                        try:
                            cookie_header, _ = cookie_manager.get_cookies_for_provider(pname)
                            cookies_cache[pname] = (cookie_header, _)
                            provider = ProviderRegistry.get(
                                pname, cookie_header, token_manager=token_manager
                            )
                            if provider:
                                result = provider.fetch_quota()
                        except Exception:
                            pass
                    return result
                return _fetch

            quota_info = dm.fetch_or_cached(
                provider_name, make_fetcher(provider_name), force=args.force
            )
            results.append(quota_info)
        else:
            # No debounce: fetch directly with auto-retry on cookie errors
            # Codex uses its own auth from ~/.codex/
            if provider_name in TOKEN_BASED_PROVIDERS:
                try:
                    provider = ProviderRegistry.get(
                        provider_name, "", token_manager=token_manager
                    )
                    if not provider:
                        results.append(QuotaInfo(provider=provider_name, error="Failed to resolve credentials"))
                    else:
                        results.append(provider.fetch_quota())
                except Exception as e:
                    results.append(QuotaInfo(provider=provider_name, error=str(e)))
                continue

            cookie_header, source = cookies_cache.get(provider_name, (None, ""))
            if not cookie_header:
                if provider_name in TOKEN_CAPABLE and token_manager.has_tokens(provider_name):
                    cookie_header = ""
                else:
                    results.append(QuotaInfo(provider=provider_name, error=source or "No credentials found"))
                    continue

            provider = ProviderRegistry.get(
                provider_name, cookie_header, token_manager=token_manager
            )
            if not provider:
                results.append(QuotaInfo(provider=provider_name, error="Unknown provider"))
                continue

            quota_info = provider.fetch_quota()

            # Auto-retry once if cookie error (cached cookies may be stale)
            if quota_info.error and is_cookie_error(quota_info.error):
                # Clear stale cookie cache and re-extract from Firefox
                clear_provider_cookie_cache(provider_name, dm.cache_dir)
                try:
                    cookie_header, source = cookie_manager.get_cookies_for_provider(provider_name)
                    cookies_cache[provider_name] = (cookie_header, source)
                    provider = ProviderRegistry.get(
                        provider_name, cookie_header, token_manager=token_manager
                    )
                    if provider:
                        quota_info = provider.fetch_quota()
                except Exception:
                    pass  # Keep original error if retry also fails

            results.append(quota_info)

    # Output results
    if args.json:
        if len(results) == 1:
            print(format_json(results[0]))
        else:
            print(format_all_json(results))
    else:
        print(format_all_rich(results, args.no_color))

    # Return error code if any provider failed
    return 1 if any(q.error for q in results) else 0


if __name__ == "__main__":
    sys.exit(main())
