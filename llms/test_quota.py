#!/usr/bin/env python3
"""Behavior tests for the Codex rate-limit reset support in quota.py.

Run: python3 -m unittest test_quota -v
"""

from __future__ import annotations

import io
import json
import os
import shutil
import sys
import tempfile
import threading
import unittest
from contextlib import redirect_stderr, redirect_stdout
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, HTTPServer
from socketserver import TCPServer
from unittest.mock import patch

import quota


def credit(credit_id: str, status: str = "available", expires_at: str | None = None) -> quota.ResetCredit:
    return quota.ResetCredit(id=credit_id, status=status, expires_at=expires_at)


# One reset that runs out soon, one that runs out later, one that never does,
# and one already spent.
HOLDINGS = [
    credit("credit-later", expires_at="2026-10-20T00:00:00Z"),
    credit("credit-sooner", expires_at="2026-10-05T00:00:00Z"),
    credit("credit-forever", expires_at=None),
    credit("credit-spent", status="redeemed", expires_at="2026-09-01T00:00:00Z"),
]


class _Recorder(BaseHTTPRequestHandler):
    """Stands in for the Codex backend and keeps what it was sent."""

    calls: list[dict] = []
    response: bytes = b'{"code":"reset","windows_reset":2}'
    holdings: bytes = b"{}"

    def _keep(self, method: str) -> None:
        length = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(length) if length else b""
        type(self).calls.append({
            "method": method,
            "path": self.path,
            "headers": dict(self.headers),
            "body": body.decode("utf-8"),
        })

    def _answer(self, payload: bytes) -> None:
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(payload)

    def do_GET(self) -> None:  # noqa: N802 - the stdlib spells it this way
        self._keep("GET")
        self._answer(type(self).holdings)

    def do_POST(self) -> None:  # noqa: N802 - the stdlib spells it this way
        self._keep("POST")
        self._answer(type(self).response)

    def log_message(self, *args: object) -> None:
        pass


class _FastHTTP(HTTPServer):
    """An HTTPServer that skips the reverse DNS lookup in server_bind.

    The stdlib calls socket.getfqdn() there, which costs seconds on a machine
    without a fast resolver. Nothing here depends on the fqdn.
    """

    def server_bind(self) -> None:
        TCPServer.server_bind(self)
        self.server_name = self.server_address[0]
        self.server_port = self.server_address[1]


class SelectCreditTests(unittest.TestCase):
    """Which reset is spent: the caller names one, or the soonest one goes."""

    def test_defaults_to_the_one_that_runs_out_first(self) -> None:
        chosen = quota.select_credit(HOLDINGS, None)
        self.assertEqual(chosen.id, "credit-sooner")

    def test_keeps_a_credit_that_never_runs_out_for_last(self) -> None:
        only_forever = [credit("forever", expires_at=None), credit("dated", expires_at="2099-01-01T00:00:00Z")]
        self.assertEqual(quota.select_credit(only_forever, None).id, "dated")
        self.assertEqual(quota.select_credit([credit("forever", expires_at=None)], None).id, "forever")

    def test_treats_an_unreadable_expiry_as_never_running_out(self) -> None:
        # The Go side reads it the same way, so both spend the same credit.
        odd = [credit("odd", expires_at="not a timestamp"),
               credit("naive", expires_at="2099-01-01T00:00:00"),
               credit("dated", expires_at="2099-02-01T00:00:00Z")]

        self.assertEqual(quota.select_credit(odd, None).id, "dated")

    def test_never_picks_a_spent_credit(self) -> None:
        spent = [credit("a", status="redeemed"), credit("b", status="redeeming")]
        with self.assertRaises(ValueError):
            quota.select_credit(spent, None)

    def test_takes_a_one_based_position(self) -> None:
        self.assertEqual(quota.select_credit(HOLDINGS, "1").id, "credit-later")
        self.assertEqual(quota.select_credit(HOLDINGS, "2").id, "credit-sooner")

    def test_refuses_a_position_whose_credit_is_spent(self) -> None:
        with self.assertRaises(ValueError) as caught:
            quota.select_credit(HOLDINGS, "4")
        self.assertIn("redeemed", str(caught.exception))

    def test_refuses_a_position_past_the_end(self) -> None:
        with self.assertRaises(ValueError):
            quota.select_credit(HOLDINGS, "9")

    def test_takes_a_credit_id(self) -> None:
        self.assertEqual(quota.select_credit(HOLDINGS, "credit-later").id, "credit-later")

    def test_refuses_an_unknown_id(self) -> None:
        with self.assertRaises(ValueError):
            quota.select_credit(HOLDINGS, "credit-nope")

    def test_refuses_an_empty_holding(self) -> None:
        with self.assertRaises(ValueError):
            quota.select_credit([], None)


class ConsumeResetTests(unittest.TestCase):
    """Spending a reset names the credit, and reaches the consume endpoint."""

    def setUp(self) -> None:
        _Recorder.calls = []
        _Recorder.response = b'{"code":"reset","windows_reset":2}'
        self.server = _FastHTTP(("127.0.0.1", 0), _Recorder)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.server.shutdown)

    def provider(self) -> quota.CodexQuotaProvider:
        base = f"http://127.0.0.1:{self.server.server_port}/wham/rate-limit-reset-credits"
        instance = quota.CodexQuotaProvider("tok-abc", "acc-123")
        instance.RESET_CREDITS_URL = base
        instance.CONSUME_URL = base + "/consume"
        return instance

    def test_posts_the_named_credit_with_an_idempotency_key(self) -> None:
        outcome = self.provider().consume_reset_credit("credit-sooner")

        self.assertEqual(outcome["code"], "reset")
        self.assertEqual(len(_Recorder.calls), 1)
        call = _Recorder.calls[0]
        self.assertEqual(call["method"], "POST")
        self.assertEqual(call["path"], "/wham/rate-limit-reset-credits/consume")

        sent = json.loads(call["body"])
        self.assertEqual(sent["credit_id"], "credit-sooner")
        self.assertTrue(sent["redeem_request_id"], "a retry would spend a second credit without a key")

    def test_carries_the_account_and_token(self) -> None:
        self.provider().consume_reset_credit("credit-sooner")

        headers = {name.lower(): value for name, value in _Recorder.calls[0]["headers"].items()}
        self.assertEqual(headers.get("authorization"), "Bearer tok-abc")
        self.assertEqual(headers.get("chatgpt-account-id"), "acc-123")
        self.assertEqual(headers.get("content-type"), "application/json")

    def test_uses_the_callers_own_idempotency_key(self) -> None:
        self.provider().consume_reset_credit("credit-sooner", redeem_request_id="redeem-777")

        self.assertEqual(json.loads(_Recorder.calls[0]["body"])["redeem_request_id"], "redeem-777")

    def test_refuses_to_spend_without_naming_a_credit(self) -> None:
        with self.assertRaises(ValueError):
            self.provider().consume_reset_credit("")
        self.assertEqual(_Recorder.calls, [], "an unnamed credit must not reach the backend")

    def test_reports_what_the_backend_said(self) -> None:
        _Recorder.response = b'{"code":"nothing_to_reset","windows_reset":0}'
        outcome = self.provider().consume_reset_credit("credit-sooner")

        self.assertEqual(outcome["code"], "nothing_to_reset")
        self.assertEqual(quota.describe_reset_outcome(outcome["code"], outcome["windows_reset"]), 
                         "nothing to reset - no window has been used, and the reset is kept")


class ConsumeUrlTests(unittest.TestCase):
    def test_consume_url_is_derived_from_the_listing_url(self) -> None:
        self.assertEqual(
            quota.CodexQuotaProvider.CONSUME_URL,
            quota.CodexQuotaProvider.RESET_CREDITS_URL + "/consume",
        )


class FetchResetCreditsTests(unittest.TestCase):
    """Reading what the account holds, the way a redeem has to see it.

    A redeem chooses a credit from this reading, so a failure must raise
    rather than come back with a blank list.
    """

    def setUp(self) -> None:
        _Recorder.calls = []
        self.server = _FastHTTP(("127.0.0.1", 0), _Recorder)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.server.shutdown)

    def provider(self) -> quota.CodexQuotaProvider:
        base = f"http://127.0.0.1:{self.server.server_port}/wham/rate-limit-reset-credits"
        instance = quota.CodexQuotaProvider("tok-abc", "acc-123")
        instance.RESET_CREDITS_URL = base
        instance.CONSUME_URL = base + "/consume"
        return instance

    def test_reads_the_credits_and_the_count(self) -> None:
        _Recorder.holdings = json.dumps({
            "credits": [
                {"id": "a", "status": "available", "expires_at": "2026-10-05T00:00:00Z"},
                {"id": "b", "status": "redeemed", "expires_at": None},
            ],
            "available_count": 1,
        }).encode()

        holdings = self.provider().fetch_reset_credits()

        self.assertEqual(holdings.reset_credit_count, 1)
        self.assertEqual([c.id for c in holdings.reset_credits], ["a", "b"])
        self.assertEqual(_Recorder.calls[0]["method"], "GET")
        self.assertEqual(quota.select_credit(holdings.reset_credits, None).id, "a")

    def test_raises_when_the_backend_refuses(self) -> None:
        _Recorder.holdings = json.dumps({"error": {"message": "token expired"}}).encode()

        with self.assertRaises(Exception) as caught:
            self.provider().fetch_reset_credits()
        self.assertIn("token expired", str(caught.exception))


class FetchQuotaTests(unittest.TestCase):
    """A reached limit is still a usage window with a known reset time."""

    def test_keeps_both_windows_when_the_limit_is_reached(self) -> None:
        now = int(datetime.now(timezone.utc).timestamp())
        usage = {
            "plan_type": "plus",
            "rate_limit": {
                "allowed": False,
                "limit_reached": True,
                "primary_window": {
                    "used_percent": 100,
                    "limit_window_seconds": 18000,
                    "reset_after_seconds": 1984,
                    "reset_at": now + 1984,
                },
                "secondary_window": {
                    "used_percent": 16,
                    "limit_window_seconds": 604800,
                    "reset_after_seconds": 544189,
                    "reset_at": now + 544189,
                },
            },
            "rate_limit_reset_credits": {"available_count": 0},
        }
        credits = {"credits": [], "available_count": 0}
        provider = quota.CodexQuotaProvider("tok-abc", "acc-123")

        def answer(url: str, **_kwargs: object) -> dict:
            return usage if url == provider.API_URL else credits

        with patch.object(provider, "_curl_request", side_effect=answer):
            info = provider.fetch_quota()

        self.assertIsNone(info.error)
        sessions = info.to_dict()["sessions"]
        self.assertEqual(sessions["current"]["used_percent"], 100)
        self.assertEqual(sessions["weekly"]["used_percent"], 16)
        self.assertTrue(sessions["current"]["resets_in"])
        self.assertTrue(sessions["weekly"]["resets_in"])


class ResetListingTests(unittest.TestCase):
    """What a person reads to choose a slot."""

    def render(self, credits: list[quota.ResetCredit], count: int | None = None) -> str:
        out = io.StringIO()
        with redirect_stdout(out):
            quota.print_reset_credits(
                quota.QuotaInfo(provider="codex", reset_credit_count=count, reset_credits=credits)
            )
        return out.getvalue()

    def test_numbers_every_credit_so_a_position_can_be_picked(self) -> None:
        rendered = self.render(HOLDINGS, count=3)

        self.assertIn("3 available", rendered)
        self.assertIn("1.", rendered)
        self.assertIn("credit-later", rendered)
        self.assertIn("redeemed", rendered, "a spent credit is still shown, marked as spent")
        self.assertIn("never", rendered, "a credit with no expiry reads as never running out")

    def test_says_so_when_the_account_holds_none(self) -> None:
        self.assertIn("No rate-limit reset", self.render([], count=0))

    def test_counts_available_credits_when_the_backend_says_nothing(self) -> None:
        # HOLDINGS holds three that can still be spent and one already spent.
        self.assertIn("3 available", self.render(HOLDINGS, count=None))


class CommandlineTests(unittest.TestCase):
    """The redeem path is Codex-only and never sneaks into a normal query."""

    def run_quota(self, *argv: str) -> tuple[int, str, str]:
        out, err = io.StringIO(), io.StringIO()
        previous = sys.argv
        sys.argv = ["quota.py", *argv]
        try:
            with redirect_stdout(out), redirect_stderr(err):
                code = quota.main()
        except SystemExit as exit_code:
            # argparse exits on --help and on an unknown flag, the way the
            # real command does.
            code = exit_code.code if isinstance(exit_code.code, int) else 1
        finally:
            sys.argv = previous
        return code, out.getvalue(), err.getvalue()

    def test_redeem_refuses_a_provider_that_has_no_resets(self) -> None:
        code, _, err = self.run_quota("-p", "claude", "--redeem")
        self.assertNotEqual(code, 0)
        self.assertIn("codex", err.lower())

    def test_redeem_refuses_without_a_provider(self) -> None:
        code, _, err = self.run_quota("--redeem")
        self.assertNotEqual(code, 0)
        self.assertIn("codex", err.lower())

    def test_list_resets_refuses_a_provider_that_has_no_resets(self) -> None:
        code, _, err = self.run_quota("-p", "kimi", "--list-resets")
        self.assertNotEqual(code, 0)
        self.assertIn("codex", err.lower())

    def test_says_how_to_fix_a_missing_codex_sign_in(self) -> None:
        # Resets belong to the ChatGPT account, and the account is reached
        # through ai-proxy's auth.json. Without it there is nothing to read.
        home = tempfile.mkdtemp()
        previous = os.environ.get("HOME")
        os.environ["HOME"] = home
        try:
            code, _, err = self.run_quota("-p", "codex", "--list-resets")
        finally:
            os.environ["HOME"] = previous
            shutil.rmtree(home, ignore_errors=True)

        self.assertNotEqual(code, 0)
        self.assertIn("login codex", err.lower(), f"the failure must name the fix, got: {err!r}")

    def test_help_names_the_codex_sign_in_it_needs(self) -> None:
        code, out, err = self.run_quota("--help")
        self.assertEqual(code, 0)
        self.assertIn("login codex", (out + err).lower(), "the help must state the prerequisite")


class DeepSeekPeakPricingTests(unittest.TestCase):
    """Peak pricing follows the published UTC hours, weekdays, and CN holidays."""

    @staticmethod
    def at(year: int, month: int, day: int, hour: int, minute: int = 0) -> datetime:
        return datetime(year, month, day, hour, minute, tzinfo=timezone.utc)

    def test_weekday_peak_windows(self) -> None:
        # 2026-10-16 is a Friday.
        self.assertTrue(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 1, 0)))
        self.assertTrue(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 3, 59)))
        self.assertTrue(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 6, 0)))
        self.assertTrue(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 9, 59)))

    def test_window_boundaries_are_off_peak(self) -> None:
        self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 4, 0)))
        self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 5, 59)))
        self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 16, 10, 0)))

    def test_weekends_are_off_peak_all_day(self) -> None:
        # 2026-10-17 is a Saturday, 2026-10-18 a Sunday.
        for hour in (0, 2, 7, 9, 12, 23):
            self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 17, hour)))
            self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 18, hour)))

    def test_chinese_public_holidays_are_off_peak(self) -> None:
        # National Day (Thursday) and Spring Festival (Monday), inside peak windows.
        self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 10, 1, 7, 0)))
        self.assertFalse(quota.DeepSeekQuotaProvider._is_peak_hour(self.at(2026, 2, 16, 2, 0)))

    def test_refresh_replaces_the_cached_flag(self) -> None:
        info = quota.QuotaInfo(provider="deepseek")
        info.sessions["monthly_cost"] = quota.SessionQuota(
            extra={"peak_valley_is_peak": False, "peak_valley_label": "Valley (1x)"}
        )
        status = {
            "is_peak": True,
            "label": "Peak (2x)",
            "now_utc_str": "07:00",
            "active_windows": ["06:00-10:00"],
            "note": "test",
        }

        with patch.object(quota.DeepSeekQuotaProvider, "_peak_valley_status", return_value=status):
            quota.refresh_peak_valley(info)

        extra = info.sessions["monthly_cost"].extra
        self.assertTrue(extra["peak_valley_is_peak"])
        self.assertEqual(extra["peak_valley_label"], "Peak (2x)")
        self.assertEqual(extra["peak_valley_windows"], ["06:00-10:00"])

    def test_refresh_leaves_other_providers_alone(self) -> None:
        info = quota.QuotaInfo(provider="claude")
        info.sessions["current"] = quota.SessionQuota(
            extra={"peak_valley_is_peak": "untouched"}
        )

        quota.refresh_peak_valley(info)

        self.assertEqual(info.sessions["current"].extra["peak_valley_is_peak"], "untouched")


if __name__ == "__main__":
    unittest.main()
