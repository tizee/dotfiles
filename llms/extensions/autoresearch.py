"""Autoresearch extension — autonomous experiment loop.

Port of pi-autoresearch (davebcn87) for this agent's extension system.
Provides three tools (InitExperiment, RunExperiment, LogExperiment),
one slash command (/autoresearch), and lifecycle hooks for auto-resume.

Auto-resume mechanism: a ``stop`` lifecycle hook returns
``SubscriberResult(continue_execution=False)`` to block the agent loop
from exiting.  The loop continues without visible stop/restart seams.

Place this file in:
  <project>/.llms/extensions/autoresearch.py   (project-local)
  ~/.config/llms/extensions/autoresearch.py    (user-global)

Then run /reload to load it.

Companion skill: examples/skills/autoresearch/SKILL.md
Copy it to .llms/skills/autoresearch/SKILL.md for the agent to learn
the experiment loop workflow.
"""

from __future__ import annotations

import json
import re
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from llms.agent.core.lifecycle import SubscriberResult
from llms.agent.extensions import ExtensionAPI
from llms.agent.slash_registry import SlashCommandAvailability
from llms.agent.tools import ToolExecutionResult

# ---------------------------------------------------------------------------
# State types
# ---------------------------------------------------------------------------

METRIC_LINE_RE = re.compile(r"^METRIC\s+(\S+)=(\S+)$", re.MULTILINE)

# Protected files that survive git revert on discard/crash.
PROTECTED_FILES = [
    "autoresearch.jsonl",
    "autoresearch.md",
    "autoresearch.ideas.md",
    "autoresearch.sh",
    "autoresearch.checks.sh",
]

# Safety limits for auto-resume.
MAX_AUTO_RESUME = 20
AUTO_RESUME_COOLDOWN_S = 60  # seconds between auto-resume attempts


@dataclass
class ExperimentState:
    """In-memory state for the current autoresearch session."""

    name: str = ""
    metric_name: str = ""
    metric_unit: str = ""
    direction: str = "lower"  # "lower" or "higher"
    best_metric: float | None = None
    results: list[dict[str, Any]] = field(default_factory=list)


@dataclass
class Runtime:
    """Per-process runtime state for the autoresearch extension."""

    active: bool = False
    experiments_this_session: int = 0
    auto_resume_count: int = 0
    last_auto_resume_time: float = 0.0
    last_run_duration: float | None = None
    state: ExperimentState = field(default_factory=ExperimentState)


# Module-level singleton — one per agent process.
_runtime = Runtime()

# Stashed reference to the ExtensionAPI for injecting resume messages.
_api: ExtensionAPI | None = None


# ---------------------------------------------------------------------------
# JSONL persistence
# ---------------------------------------------------------------------------


def _jsonl_path(cwd: Path) -> Path:
    return cwd / "autoresearch.jsonl"


def _append_jsonl(cwd: Path, record: dict[str, Any]) -> None:
    with _jsonl_path(cwd).open("a") as f:
        f.write(json.dumps(record) + "\n")


def _reconstruct_state(cwd: Path) -> ExperimentState:
    """Reconstruct experiment state from autoresearch.jsonl."""
    state = ExperimentState()
    path = _jsonl_path(cwd)
    if not path.exists():
        return state

    for line in path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            record = json.loads(line)
        except json.JSONDecodeError:
            continue

        if record.get("type") == "config":
            state.name = record.get("name", "")
            state.metric_name = record.get("metric_name", "")
            state.metric_unit = record.get("metric_unit", "")
            state.direction = record.get("direction", "lower")
            # Reset on re-init config line.
            state.best_metric = None
            state.results.clear()
        elif "status" in record and "metric_value" in record:
            state.results.append(record)
            # Track best metric.
            val = record["metric_value"]
            if isinstance(val, (int, float)) and val > 0:
                if (
                    state.best_metric is None
                    or state.direction == "lower"
                    and val < state.best_metric
                    or state.direction == "higher"
                    and val > state.best_metric
                ):
                    state.best_metric = val

    return state


def _format_duration(seconds: float) -> str:
    """Format a duration with appropriate unit and precision."""
    if seconds >= 60:
        return f"{seconds:.1f}s"
    if seconds >= 1:
        return f"{seconds:.3f}s"
    if seconds >= 0.001:
        return f"{seconds * 1_000:.3f}ms"
    if seconds >= 0.000_001:
        return f"{seconds * 1_000_000:.3f}us"
    return f"{seconds * 1_000_000_000:.3f}ns"


def _parse_metric_lines(output: str) -> dict[str, float]:
    """Parse METRIC name=value lines from command output."""
    metrics: dict[str, float] = {}
    for match in METRIC_LINE_RE.finditer(output):
        name = match.group(1)
        try:
            metrics[name] = float(match.group(2))
        except ValueError:
            continue
    return metrics


# ---------------------------------------------------------------------------
# Tool handlers
# ---------------------------------------------------------------------------


async def _handle_init_experiment(arguments: dict[str, Any], ctx: Any) -> str:
    global _runtime

    name = arguments["name"]
    metric_name = arguments["metric_name"]
    metric_unit = arguments.get("metric_unit", "")
    direction = arguments.get("direction", "lower")

    if direction not in ("lower", "higher"):
        return f"Error: direction must be 'lower' or 'higher', got '{direction}'"

    is_reinit = len(_runtime.state.results) > 0

    _runtime.state.name = name
    _runtime.state.metric_name = metric_name
    _runtime.state.metric_unit = metric_unit
    _runtime.state.direction = direction
    _runtime.state.best_metric = None
    if is_reinit:
        _runtime.state.results.clear()

    # Write config header to JSONL.
    config_record = {
        "type": "config",
        "name": name,
        "metric_name": metric_name,
        "metric_unit": metric_unit,
        "direction": direction,
    }
    jsonl = _jsonl_path(ctx.cwd)
    try:
        if is_reinit:
            _append_jsonl(ctx.cwd, config_record)
        else:
            jsonl.write_text(json.dumps(config_record) + "\n")
    except OSError as e:
        return f"Error: failed to write autoresearch.jsonl: {e}"

    _runtime.active = True

    reinit_note = (
        " (re-initialized — previous results archived, new baseline needed)"
        if is_reinit
        else ""
    )
    return (
        f'Experiment initialized: "{name}"{reinit_note}\n'
        f"Metric: {metric_name} ({metric_unit or 'unitless'}, {direction} is better)\n"
        f"Config written to autoresearch.jsonl. Now run the baseline with RunExperiment."
    )


async def _handle_run_experiment(
    arguments: dict[str, Any], ctx: Any
) -> str | ToolExecutionResult:
    global _runtime

    command = arguments["command"]
    timeout = min(max(arguments.get("timeout", 600), 1), 3600)

    t0 = time.monotonic()
    try:
        result = await ctx.execute_bash(command, timeout=timeout)
    except TimeoutError:
        elapsed = time.monotonic() - t0
        _runtime.last_run_duration = elapsed
        return f"TIMEOUT after {_format_duration(elapsed)}\nCommand: {command}"

    elapsed = time.monotonic() - t0
    _runtime.last_run_duration = elapsed

    output = result.output
    exit_code = result.exit_code
    passed = exit_code == 0

    # Parse METRIC lines.
    parsed_metrics = _parse_metric_lines(output)
    primary_value = parsed_metrics.get(_runtime.state.metric_name)

    # Build response.
    lines: list[str] = []

    if not passed:
        lines.append(f"FAILED (exit code {exit_code}) in {_format_duration(elapsed)}")
    else:
        lines.append(f"PASSED in {_format_duration(elapsed)}")

    if _runtime.state.best_metric is not None:
        unit = _runtime.state.metric_unit
        lines.append(
            f"Current best {_runtime.state.metric_name}: {_runtime.state.best_metric}{unit}"
        )

    if parsed_metrics:
        parts = []
        for name, value in parsed_metrics.items():
            marker = " (primary)" if name == _runtime.state.metric_name else ""
            parts.append(f"{name}={value}{marker}")
        lines.append(f"Parsed metrics: {', '.join(parts)}")

        if primary_value is not None:
            secondary = {
                k: v
                for k, v in parsed_metrics.items()
                if k != _runtime.state.metric_name
            }
            secondary_str = ", ".join(f'"{k}": {v}' for k, v in secondary.items())
            lines.append(
                f"Use these values in LogExperiment: metric_value={primary_value}"
                + (f", secondary_metrics={{{secondary_str}}}" if secondary_str else "")
            )

    # Truncate output for context conservation (last 50 lines, max 8KB).
    output_lines = output.splitlines()
    max_lines = 50
    max_bytes = 8192
    truncated_output = "\n".join(output_lines[-max_lines:])
    if len(truncated_output) > max_bytes:
        truncated_output = truncated_output[-max_bytes:]

    if len(output_lines) > max_lines:
        lines.append(f"\n[Showing last {max_lines} of {len(output_lines)} lines]")

    lines.append(f"\n{truncated_output}")

    return "\n".join(lines)


async def _handle_log_experiment(arguments: dict[str, Any], ctx: Any) -> str:
    global _runtime

    status = arguments["status"]
    metric_value = arguments["metric_value"]
    description = arguments["description"]
    secondary_metrics = arguments.get("secondary_metrics", {})

    if status not in ("keep", "discard", "crash"):
        return f"Error: status must be 'keep', 'discard', or 'crash', got '{status}'"

    state = _runtime.state

    # Build experiment record.
    experiment = {
        "run": len(state.results) + 1,
        "status": status,
        "metric_value": metric_value,
        "secondary_metrics": secondary_metrics,
        "description": description,
        "timestamp": int(time.time()),
        "wall_clock_s": _runtime.last_run_duration,
    }

    state.results.append(experiment)
    _runtime.experiments_this_session += 1

    # Track best metric.
    if isinstance(metric_value, (int, float)) and metric_value > 0 and status == "keep":
        if (
            state.best_metric is None
            or state.direction == "lower"
            and metric_value < state.best_metric
            or state.direction == "higher"
            and metric_value > state.best_metric
        ):
            state.best_metric = metric_value

    # Build response text.
    lines: list[str] = []
    lines.append(f"Logged #{len(state.results)}: {status} -- {description}")

    if state.best_metric is not None:
        unit = state.metric_unit
        lines.append(f"Baseline {state.metric_name}: {state.best_metric}{unit}")
        if status == "keep" and metric_value > 0 and len(state.results) > 1:
            delta = metric_value - state.best_metric
            if state.best_metric != 0:
                pct = (delta / state.best_metric) * 100
                sign = "+" if delta > 0 else ""
                lines.append(f"This: {metric_value}{unit} ({sign}{pct:.1f}%)")

    if secondary_metrics:
        parts = [f"{k}: {v}" for k, v in secondary_metrics.items()]
        lines.append(f"Secondary: {', '.join(parts)}")

    lines.append(f"({len(state.results)} experiments total)")

    # Git operations: commit on keep, revert on discard/crash.
    if status == "keep":
        try:
            # Stage and commit.
            commit_msg = f"autoresearch: {description}"
            add_result = await ctx.execute_bash("git add -A", timeout=10)
            if add_result.exit_code != 0:
                lines.append(f"Warning: git add failed: {add_result.output[:200]}")
            else:
                diff_result = await ctx.execute_bash(
                    "git diff --cached --quiet", timeout=10
                )
                if diff_result.exit_code == 0:
                    lines.append("Git: nothing to commit (working tree clean)")
                else:
                    commit_result = await ctx.execute_bash(
                        f'git commit -m "{commit_msg}"', timeout=10
                    )
                    if commit_result.exit_code == 0:
                        first_line = (
                            commit_result.output.splitlines()[0]
                            if commit_result.output
                            else ""
                        )
                        lines.append(f"Git: committed -- {first_line}")
                    else:
                        lines.append(
                            f"Warning: git commit failed: {commit_result.output[:200]}"
                        )
        except Exception as e:
            lines.append(f"Warning: git error: {e}")
    else:
        # Revert on discard/crash — protect autoresearch files.
        try:
            protect_cmds = "; ".join(
                f'git add "{f}" 2>/dev/null || true' for f in PROTECTED_FILES
            )
            revert_cmd = (
                f"{protect_cmds}; git checkout -- . ; git clean -fd 2>/dev/null"
            )
            await ctx.execute_bash(revert_cmd, timeout=10)
            lines.append(
                f"Git: reverted changes ({status}) -- autoresearch files preserved"
            )
        except Exception as e:
            lines.append(f"Warning: git revert failed: {e}")

    # Persist to JSONL (always, regardless of status).
    try:
        _append_jsonl(ctx.cwd, experiment)
    except OSError as e:
        lines.append(f"Warning: failed to write autoresearch.jsonl: {e}")

    _runtime.last_run_duration = None

    # Reinforce the loop: always remind the agent to continue.
    lines.append("\nContinue experimenting. Do NOT stop or ask whether to continue.")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Lifecycle hooks
# ---------------------------------------------------------------------------


def _on_stop(event: Any) -> SubscriberResult:
    """Block stopping when autoresearch mode is active and experiments have run.

    Returns ``SubscriberResult(continue_execution=False)`` to keep the agent
    loop running, and injects a resume prompt so the agent knows to continue
    the experiment loop (especially useful after context compaction).

    Safety limits:
    - MAX_AUTO_RESUME: hard cap on resume attempts.  When reached, the loop
      deactivates and injects a final summary so the agent knows why it stopped.
    - Cooldown: prevents rapid-fire resumes.  Still counts toward the limit.
    """
    if not _runtime.active:
        return SubscriberResult()
    if _runtime.experiments_this_session == 0:
        return SubscriberResult()

    # Safety limit reached — deactivate and let the agent stop naturally.
    if _runtime.auto_resume_count >= MAX_AUTO_RESUME:
        _runtime.active = False
        return SubscriberResult()

    # Cooldown: let the agent stop this time without counting toward the limit.
    now = time.monotonic()
    if now - _runtime.last_auto_resume_time < AUTO_RESUME_COOLDOWN_S:
        return SubscriberResult()

    _runtime.auto_resume_count += 1
    _runtime.last_auto_resume_time = now

    # Inject a resume prompt for the next turn.
    if _api is not None:
        resume_msg = (
            "Continue the autoresearch experiment loop. "
            "Read autoresearch.md and git log for context if needed."
        )
        ideas_path = Path.cwd() / "autoresearch.ideas.md"
        if ideas_path.exists():
            resume_msg += " Check autoresearch.ideas.md for promising paths."
        _api.send_user_message(resume_msg)

    return SubscriberResult(continue_execution=False)


def _on_session_start(event: Any) -> None:
    """Reconstruct state from JSONL when a session begins."""
    global _runtime

    cwd = Path.cwd()
    if _jsonl_path(cwd).exists():
        _runtime.state = _reconstruct_state(cwd)
        if _runtime.state.name:
            _runtime.active = True
            _runtime.experiments_this_session = 0
            _runtime.auto_resume_count = 0


# ---------------------------------------------------------------------------
# Slash command
# ---------------------------------------------------------------------------


def _handle_autoresearch_command(ctx: Any) -> None:
    """Handle /autoresearch [off|clear|<text>]."""
    global _runtime

    args = ctx.args.strip() if ctx.args else ""

    if args == "off":
        _runtime.active = False
        ctx.send_result("Autoresearch mode OFF. Data preserved.")
        return

    if args == "clear":
        _runtime.active = False
        _runtime.state = ExperimentState()
        _runtime.experiments_this_session = 0
        _runtime.auto_resume_count = 0
        cwd = Path.cwd()
        jsonl = _jsonl_path(cwd)
        if jsonl.exists():
            jsonl.unlink()
        ctx.send_result("Autoresearch data cleared.")
        return

    if not args:
        ctx.send_result(
            "Usage: /autoresearch [off|clear|<goal>]\n\n"
            "<goal>  Enter autoresearch mode and start/resume the loop.\n"
            "off     Leave autoresearch mode (data preserved).\n"
            "clear   Delete autoresearch.jsonl and turn off."
        )
        return

    # No-op if already active — re-entering would stack duplicate resume
    # prompts and re-import the skill, which is undefined behavior.
    if _runtime.active:
        ctx.send_result(
            "Autoresearch loop is already running. Use /autoresearch off to stop."
        )
        return

    # Enter autoresearch mode with a goal.
    _runtime.active = True
    _runtime.experiments_this_session = 0
    _runtime.auto_resume_count = 0

    # Reconstruct state if JSONL exists.
    cwd = Path.cwd()
    if _jsonl_path(cwd).exists():
        _runtime.state = _reconstruct_state(cwd)

    # Send as user message so the agent sees the goal and starts the loop.
    ctx.send_user_message(
        f"Start autoresearch: {args}\n\n"
        "Use the autoresearch skill for the experiment loop workflow. "
        "If autoresearch.md exists, resume from where we left off."
    )


# ---------------------------------------------------------------------------
# Tool schemas
# ---------------------------------------------------------------------------

INIT_EXPERIMENT_PARAMS = {
    "type": "object",
    "properties": {
        "name": {
            "type": "string",
            "description": 'Human-readable session name (e.g. "Optimizing build speed")',
        },
        "metric_name": {
            "type": "string",
            "description": 'Primary metric name (e.g. "total_ms", "val_bpb", "bundle_kb")',
        },
        "metric_unit": {
            "type": "string",
            "description": 'Metric unit (e.g. "ms", "s", "kb"). Default: ""',
        },
        "direction": {
            "type": "string",
            "description": '"lower" or "higher" — which direction is better. Default: "lower"',
            "enum": ["lower", "higher"],
        },
    },
    "required": ["name", "metric_name"],
    "additionalProperties": False,
}

RUN_EXPERIMENT_PARAMS = {
    "type": "object",
    "properties": {
        "command": {
            "type": "string",
            "description": "Shell command to run (e.g. 'bash autoresearch.sh', 'uv run train.py')",
        },
        "timeout": {
            "type": "integer",
            "description": "Kill after this many seconds (default: 600, max: 3600)",
        },
    },
    "required": ["command"],
    "additionalProperties": False,
}

LOG_EXPERIMENT_PARAMS = {
    "type": "object",
    "properties": {
        "status": {
            "type": "string",
            "description": "Experiment outcome",
            "enum": ["keep", "discard", "crash"],
        },
        "metric_value": {
            "type": "number",
            "description": "Primary metric value. Use 0 for crashes.",
        },
        "description": {
            "type": "string",
            "description": "Short description of what this experiment tried",
        },
        "secondary_metrics": {
            "type": "object",
            "description": 'Additional metrics as {"name": value} pairs for tradeoff monitoring',
            "additionalProperties": {"type": "number"},
        },
    },
    "required": ["status", "metric_value", "description"],
    "additionalProperties": False,
}


# ---------------------------------------------------------------------------
# System prompts (tool guidance)
# ---------------------------------------------------------------------------

INIT_EXPERIMENT_GUIDANCE = """\
Initialize the experiment session. Call once before the first RunExperiment
to set the session name, primary metric, unit, and optimization direction.
Writes the config header to autoresearch.jsonl.

Guidelines:
- Call InitExperiment exactly once at the start of an autoresearch session.
- If autoresearch.jsonl already exists with a config, do NOT call InitExperiment again.
- If the optimization target changes, call InitExperiment again to reset."""

RUN_EXPERIMENT_GUIDANCE = """\
Run a shell command as a timed experiment. Captures wall-clock duration,
output, and exit code. Parses METRIC name=value lines from stdout.

Guidelines:
- Use RunExperiment instead of Bash when running experiment commands.
- After RunExperiment, always call LogExperiment to record the result.
- If the benchmark script outputs METRIC lines (e.g. 'METRIC total_ms=152'),
  they are parsed automatically. Use parsed values in LogExperiment."""

LOG_EXPERIMENT_GUIDANCE = """\
Record an experiment result. Auto-commits on 'keep', auto-reverts on
'discard'/'crash'. Appends to autoresearch.jsonl.

Guidelines:
- Always call LogExperiment after RunExperiment.
- Git operations are automatic: keep=commit, discard/crash=revert.
  Do NOT commit or revert manually.
- Use 'keep' if the primary metric improved.
  'discard' if worse or unchanged. 'crash' if it failed.
- Append promising but deferred ideas to autoresearch.ideas.md."""


# ---------------------------------------------------------------------------
# Extension setup
# ---------------------------------------------------------------------------


def setup(api: ExtensionAPI) -> None:
    global _api
    _api = api

    # Tools
    api.register_tool(
        name="InitExperiment",
        description=(
            "Initialize autoresearch experiment session. "
            "Sets name, primary metric, unit, and optimization direction."
        ),
        parameters=INIT_EXPERIMENT_PARAMS,
        execute=_handle_init_experiment,
        system_prompt=INIT_EXPERIMENT_GUIDANCE,
    )

    api.register_tool(
        name="RunExperiment",
        description=(
            "Run a timed experiment command. "
            "Captures duration, output, exit code, and parses METRIC lines."
        ),
        parameters=RUN_EXPERIMENT_PARAMS,
        execute=_handle_run_experiment,
        system_prompt=RUN_EXPERIMENT_GUIDANCE,
    )

    api.register_tool(
        name="LogExperiment",
        description=(
            "Record experiment result. "
            "Auto-commits on keep, auto-reverts on discard/crash."
        ),
        parameters=LOG_EXPERIMENT_PARAMS,
        execute=_handle_log_experiment,
        system_prompt=LOG_EXPERIMENT_GUIDANCE,
    )

    # Slash command — ALWAYS available so /autoresearch off works while the
    # agent loop is kept alive by the stop hook.
    api.register_command(
        name="autoresearch",
        description="Enter/exit autonomous experiment loop mode",
        handler=_handle_autoresearch_command,
        availability=SlashCommandAvailability.ALWAYS,
        argument_hint="[off|clear|<goal>]",
    )

    # Lifecycle hooks
    api.on("stop", _on_stop)
    api.on("session_start", _on_session_start)
