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
class QuotaInfo:
    """Unified quota information from a provider"""

    provider: str
    account: Optional[str] = None
    plan_type: Optional[str] = None  # e.g., "free", "pro", "enterprise"
    sessions: dict[str, SessionQuota] = field(default_factory=dict)
    # Common session keys: "current" (5-hour), "weekly", "weekly_opus", "weekly_sonnet"
    raw_response: Optional[dict[str, Any]] = None
    error: Optional[str] = None
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
        if self.raw_response:
            result["raw_response"] = self.raw_response
        if self.error:
            result["error"] = self.error
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

        return cls(
            provider=data.get("provider", "unknown"),
            account=data.get("account"),
            plan_type=data.get("plan_type"),
            sessions=sessions,
            raw_response=data.get("raw_response"),
            error=data.get("error"),
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
    """Claude Code quota provider"""

    name = "claude"
    description = "Claude Code (claude.ai)"
    BASE_URL = "https://claude.ai/api"

    def __init__(self, cookie_header: str):
        super().__init__(cookie_header)

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

        # Add local time and duration
        if resets_at:
            quota.resets_at_local, quota.resets_in = self._format_reset_time(resets_at)

        return quota

    def _format_reset_time(self, resets_at: str) -> tuple[Optional[str], Optional[str]]:
        """Format reset time to local timezone and human-readable duration"""
        try:
            dt = datetime.fromisoformat(resets_at.replace("Z", "+00:00"))
            local_tz = self._get_system_timezone()
            local_dt = dt.astimezone(local_tz)
            tz_name = local_dt.tzname() or "Local"

            local_str = local_dt.strftime(f"%H:%M")

            now = datetime.now(timezone.utc)
            delta = dt - now
            total_seconds = int(delta.total_seconds())

            if total_seconds <= 0:
                duration_str = "now"
            else:
                hours, remainder = divmod(total_seconds, 3600)
                minutes = remainder // 60
                if hours > 0:
                    duration_str = f"{hours}h {minutes}m"
                else:
                    duration_str = f"{minutes}m"

            return local_str, duration_str
        except Exception:
            return None, None

    def _get_system_timezone(self) -> timezone:
        """Get system local timezone"""
        try:
            local_offset = -time.timezone
            if time.daylight and time.localtime().tm_isdst > 0:
                local_offset += 3600
            return timezone(timedelta(seconds=local_offset))
        except Exception:
            return timezone(timedelta(hours=8))

    def fetch_quota(self) -> QuotaInfo:
        """Fetch Claude quota"""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            # Fetch organizations
            orgs_data = self._curl_request(f"{self.BASE_URL}/organizations")
            orgs = orgs_data if isinstance(orgs_data, list) else [orgs_data]
            org = self._select_organization(orgs)

            # Fetch usage
            usage_data = self._curl_request(
                f"{self.BASE_URL}/organizations/{org['uuid']}/usage"
            )

            quota_info.raw_response = usage_data

            # Parse quotas
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

            # Extra usage credits
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

        except Exception as e:
            quota_info.error = str(e)

        return quota_info


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
                    used = model.get("current_interval_usage_count", 0)
                    remains_ms = model.get("remains_time", 0)
                    end_time_ms = model.get("end_time", 0)

                    # Calculate percentages (total and used are in seconds)
                    used_percent = (used / total * 100) if total > 0 else None
                    remaining_percent = ((total - used) / total * 100) if total > 0 else None

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
                        local_tz = self._get_system_timezone()
                        local_dt = dt.astimezone(local_tz)
                        resets_at_local = local_dt.strftime("%H:%M")

                        # Time until reset
                        now = datetime.now(timezone.utc)
                        delta = dt - now
                        total_seconds = int(delta.total_seconds())
                        if total_seconds <= 0:
                            resets_in = "now"
                        else:
                            hours, remainder = divmod(total_seconds, 3600)
                            minutes = remainder // 60
                            if hours > 0:
                                resets_in = f"{hours}h {minutes}m"
                            else:
                                resets_in = f"{minutes}m"

                    quota_info.sessions[session_key] = SessionQuota(
                        used_percent=used_percent,
                        remaining_percent=remaining_percent,
                        resets_at=resets_at,
                        resets_at_local=resets_at_local,
                        resets_in=resets_in,
                        limit=total,  # in seconds
                        used=used,    # in seconds
                        extra={
                            "model_name": model_name,
                            "remains_seconds": remains_seconds,
                        },
                    )
            else:
                error_msg = data.get("base_resp", {}).get("status_msg", "Unknown error")
                quota_info.error = error_msg

        except Exception as e:
            quota_info.error = str(e)

        return quota_info

    def _get_system_timezone(self) -> timezone:
        """Get system local timezone"""
        try:
            local_offset = -time.timezone
            if time.daylight and time.localtime().tm_isdst > 0:
                local_offset += 3600
            return timezone(timedelta(seconds=local_offset))
        except Exception:
            return timezone(timedelta(hours=8))


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
                        local_tz = self._get_system_timezone()
                        local_dt = dt.astimezone(local_tz)
                        resets_at_local = local_dt.strftime("%H:%M")
                        now = datetime.now(timezone.utc)
                        delta = dt - now
                        total_seconds = int(delta.total_seconds())
                        if total_seconds <= 0:
                            resets_in = "now"
                        else:
                            hours, remainder = divmod(total_seconds, 3600)
                            minutes = remainder // 60
                            if hours > 0:
                                resets_in = f"{hours}h {minutes}m"
                            else:
                                resets_in = f"{minutes}m"

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

        except Exception as e:
            quota_info.error = str(e)

        return quota_info

    def _get_system_timezone(self) -> timezone:
        """Get system local timezone"""
        try:
            local_offset = -time.timezone
            if time.daylight and time.localtime().tm_isdst > 0:
                local_offset += 3600
            return timezone(timedelta(seconds=local_offset))
        except Exception:
            return timezone(timedelta(hours=8))


# ============================================================================
# Kimi Provider Implementation
# ============================================================================


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
            local_tz = self._get_system_timezone()
            local_dt = dt.astimezone(local_tz)
            resets_at_local = local_dt.strftime("%H:%M")

            # Time until reset
            now = datetime.now(timezone.utc)
            delta = dt - now
            total_seconds = int(delta.total_seconds())
            if total_seconds <= 0:
                resets_in = "now"
            else:
                hours, remainder = divmod(total_seconds, 3600)
                minutes = remainder // 60
                if hours > 0:
                    resets_in = f"{hours}h {minutes}m"
                else:
                    resets_in = f"{minutes}m"

            return resets_at, resets_at_local, resets_in
        except Exception:
            return None, None, None

    def _get_system_timezone(self) -> timezone:
        """Get system local timezone"""
        try:
            local_offset = -time.timezone
            if time.daylight and time.localtime().tm_isdst > 0:
                local_offset += 3600
            return timezone(timedelta(seconds=local_offset))
        except Exception:
            return timezone(timedelta(hours=8))

    def fetch_quota(self) -> QuotaInfo:
        """Fetch Kimi quota"""
        quota_info = QuotaInfo(
            provider=self.name,
            fetched_at=datetime.now(timezone.utc).isoformat(),
        )

        try:
            data = self._curl_request(self.API_URL)
            quota_info.raw_response = data

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

    @classmethod
    def get(cls, name: str, cookie_header: str) -> Optional[QuotaProvider]:
        """Get a provider instance by name"""
        provider_class = cls._providers.get(name)
        if provider_class:
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


# ============================================================================
# Cookie Manager - Firefox Cookie Extraction
# ============================================================================


class CookieManager:
    """Manages cookie extraction from Firefox"""

    def __init__(self, profile: Optional[str] = None):
        self.profile = profile

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
        host_patterns = {
            "claude": "claude.ai",
            "minimax": "minimaxi.com",
            "glm": "bigmodel.cn",
            "kimi": "kimi.com",
        }

        host_pattern = host_patterns.get(provider_name)
        if not host_pattern:
            raise Exception(f"Unknown provider: {provider_name}")

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

                header = self.build_cookie_header(cookies)
                return header, f"Firefox profile: {prof.name}"

        raise Exception(f"No valid credentials found for {provider_name}")


# ============================================================================
# Output Formatters
# ============================================================================


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
    """Format reset time as readable date (e.g., 'Mar 5 @ 01:37 (in 2h 30m)')"""
    if not resets_at:
        if resets_in:
            return f"Resets in: {resets_in}"
        return ""
    
    try:
        # Parse ISO date and format as "Mar 5 @ 01:37"
        dt = datetime.fromisoformat(resets_at.replace("Z", "+00:00"))
        date_str = dt.strftime("%b %-d")
        # On macOS, %-d doesn't work, use %d and strip leading zero
        date_str = dt.strftime("%b %d").replace(" 0", " ")
        time_str = resets_at_local or dt.strftime("%H:%M")
        
        result = f"Resets: {date_str} @ {time_str}"
        if resets_in:
            result += f" (in {resets_in})"
        return result
    except Exception:
        if resets_at_local and resets_in:
            return f"Resets: {resets_at_local} (in {resets_in})"
        elif resets_in:
            return f"Resets in: {resets_in}"
        return ""


def format_rich(quota_info: QuotaInfo, no_color: bool = False) -> str:
    """Format quota info for rich terminal display"""
    lines = []
    lines.append("")
    lines.append("=" * 55)
    lines.append(f"  {quota_info.provider.upper()} Coding Plan Quota")
    lines.append("=" * 55)

    if quota_info.error:
        lines.append(f"\n  Error: {quota_info.error}")
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
        
        if has_prompt_counts:
            # Show prompt usage
            if session.currency:
                # Currency-based usage (like Claude extra credits)
                lines.append(f"   Usage: {session.currency} ${session.used:.2f} / ${session.limit:.2f}")
            else:
                # Count-based usage (like prompt count)
                used_count = session.used
                limit_count = session.limit
                # Show percentage in the usage line
                pct_str = f" ({session.used_percent:.1f}%)" if session.used_percent is not None else ""
                lines.append(f"   Prompts: {used_count:.0f} / {limit_count:.0f}{pct_str}")
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

        if not has_prompt_counts and session.used_percent is None and not reset_str:
            lines.append("   Status: No data")

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
        self.cache_dir.mkdir(parents=True, exist_ok=True)

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
        """Clear cache for provider or all"""
        if provider:
            for suffix in (".json", ".lock"):
                path = self.cache_dir / f"{provider}{suffix}"
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
    MINIMAX_API_KEY   MiniMax API key (if not using cookies)
    NO_COLOR          Disable colored output
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

    # Initialize debounce manager
    dm = DebounceManager(interval_seconds=args.debounce) if args.debounce > 0 else None

    # Handle --clear-cache
    if args.clear_cache:
        if dm:
            dm.clear_cache(provider=args.provider)
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
    results: list[QuotaInfo] = []

    for provider_name in providers_to_query:
        if dm:
            # Debounce mode: use lock-based fetch_or_cached to deduplicate
            # concurrent API calls from multiple agents
            def make_fetcher(pname: str) -> Callable[[], QuotaInfo]:
                def _fetch() -> QuotaInfo:
                    try:
                        cookie_header, source = cookie_manager.get_cookies_for_provider(pname)
                        if not args.json:
                            print(f"Credentials: {source}", file=sys.stderr)
                    except Exception as e:
                        return QuotaInfo(provider=pname, error=str(e))
                    provider = ProviderRegistry.get(pname, cookie_header)
                    if not provider:
                        return QuotaInfo(provider=pname, error="Unknown provider")
                    return provider.fetch_quota()
                return _fetch

            quota_info = dm.fetch_or_cached(
                provider_name, make_fetcher(provider_name), force=args.force
            )
            results.append(quota_info)
        else:
            # No debounce: fetch directly
            try:
                cookie_header, source = cookie_manager.get_cookies_for_provider(provider_name)
                if not args.json:
                    print(f"Credentials: {source}", file=sys.stderr)
            except Exception as e:
                results.append(QuotaInfo(provider=provider_name, error=str(e)))
                continue

            provider = ProviderRegistry.get(provider_name, cookie_header)
            if not provider:
                results.append(QuotaInfo(provider=provider_name, error="Unknown provider"))
                continue

            results.append(provider.fetch_quota())

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
