"""Tests for git operations: clone_plugin, update_plugin, source_plugins."""

import stat
import subprocess
from unittest import mock

from . import conftest

rtpm = conftest.rtpm
PluginSpec = rtpm.PluginSpec


class TestClonePlugin:
    def test_already_exists(self, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        (plugin_dir / "my-plugin").mkdir()

        spec = PluginSpec(name="my-plugin", repo="user/my-plugin")
        assert rtpm.clone_plugin(spec, plugin_dir) is True

    @mock.patch("subprocess.run")
    def test_clone_success(self, mock_run, tmp_path):
        mock_run.return_value = subprocess.CompletedProcess([], 0)
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()

        spec = PluginSpec(name="my-plugin", repo="user/my-plugin")
        assert rtpm.clone_plugin(spec, plugin_dir) is True
        call_args = mock_run.call_args[0][0]
        assert "git" in call_args
        assert "clone" in call_args
        assert "--depth=1" in call_args
        assert "--single-branch" in call_args
        assert "--recursive" in call_args
        assert "--shallow-submodules" in call_args

    @mock.patch("subprocess.run")
    def test_clone_with_branch(self, mock_run, tmp_path):
        mock_run.return_value = subprocess.CompletedProcess([], 0)
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()

        spec = PluginSpec(name="repo", repo="user/repo", branch="develop")
        rtpm.clone_plugin(spec, plugin_dir)
        call_args = mock_run.call_args[0][0]
        assert "-b" in call_args
        assert "develop" in call_args
        assert "--depth=1" in call_args

    @mock.patch("subprocess.run")
    def test_clone_failure(self, mock_run, tmp_path):
        mock_run.return_value = subprocess.CompletedProcess([], 128, stderr="fail")
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()

        spec = PluginSpec(name="bad", repo="user/bad")
        assert rtpm.clone_plugin(spec, plugin_dir) is False


class TestUpdatePlugin:
    """Tests for the resetupdate strategy: fetch --depth=1 + reset --hard FETCH_HEAD."""

    @mock.patch("subprocess.run")
    def test_not_installed(self, mock_run, tmp_path):
        ok, msg = rtpm.update_plugin("missing", tmp_path)
        assert ok is False
        assert "not installed" in msg

    @mock.patch("subprocess.run")
    def test_update_success(self, mock_run, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)
        mock_run.return_value = subprocess.CompletedProcess(
            [], 0, stdout="HEAD is now at abc1234\n", stderr=""
        )
        ok, msg = rtpm.update_plugin("repo", plugin_dir)
        assert ok is True

    @mock.patch("subprocess.run")
    def test_update_uses_fetch_depth1_and_reset(self, mock_run, tmp_path):
        """Verify the resetupdate strategy: fetch --depth=1 then reset --hard FETCH_HEAD."""
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)
        mock_run.return_value = subprocess.CompletedProcess(
            [], 0, stdout="", stderr=""
        )
        rtpm.update_plugin("repo", plugin_dir)

        # Collect all git command sequences
        git_calls = [call[0][0] for call in mock_run.call_args_list]

        # First call: symbolic-ref to detect current branch
        assert "symbolic-ref" in git_calls[0]

        # Second call: fetch --depth=1
        fetch_call = git_calls[1]
        assert "fetch" in fetch_call
        assert "--depth=1" in fetch_call
        assert "origin" in fetch_call

        # Third call: reset --hard FETCH_HEAD
        reset_call = git_calls[2]
        assert "reset" in reset_call
        assert "--hard" in reset_call
        assert "FETCH_HEAD" in reset_call

    @mock.patch("subprocess.run")
    def test_update_with_detected_branch(self, mock_run, tmp_path):
        """Branch detected from HEAD is passed to fetch."""
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)

        def side_effect(args, **kwargs):
            if "symbolic-ref" in args:
                return subprocess.CompletedProcess(args, 0, stdout="main\n", stderr="")
            return subprocess.CompletedProcess(args, 0, stdout="", stderr="")

        mock_run.side_effect = side_effect
        rtpm.update_plugin("repo", plugin_dir)

        fetch_call = mock_run.call_args_list[1][0][0]
        assert "main" in fetch_call

    @mock.patch("subprocess.run")
    def test_update_with_explicit_branch(self, mock_run, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)
        mock_run.return_value = subprocess.CompletedProcess(
            [], 0, stdout="", stderr=""
        )
        rtpm.update_plugin("repo", plugin_dir, branch="develop")

        # Should NOT call symbolic-ref when branch is explicit
        fetch_call = mock_run.call_args_list[0][0][0]
        assert "fetch" in fetch_call
        assert "develop" in fetch_call

    @mock.patch("subprocess.run")
    def test_update_fetch_failure(self, mock_run, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)

        def side_effect(args, **kwargs):
            if "symbolic-ref" in args:
                return subprocess.CompletedProcess(args, 0, stdout="main\n", stderr="")
            if "fetch" in args:
                return subprocess.CompletedProcess(args, 1, stdout="", stderr="fatal: repo not found")
            return subprocess.CompletedProcess(args, 0, stdout="", stderr="")

        mock_run.side_effect = side_effect
        ok, msg = rtpm.update_plugin("repo", plugin_dir)
        assert ok is False
        assert "repo not found" in msg

    @mock.patch("subprocess.run")
    def test_update_reset_failure(self, mock_run, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)

        def side_effect(args, **kwargs):
            if "symbolic-ref" in args:
                return subprocess.CompletedProcess(args, 0, stdout="main\n", stderr="")
            if "reset" in args:
                return subprocess.CompletedProcess(args, 1, stdout="", stderr="error: reset failed")
            return subprocess.CompletedProcess(args, 0, stdout="", stderr="")

        mock_run.side_effect = side_effect
        ok, msg = rtpm.update_plugin("repo", plugin_dir)
        assert ok is False
        assert "reset failed" in msg

    @mock.patch("subprocess.run")
    def test_update_shallow_submodules(self, mock_run, tmp_path):
        """Submodule update should also use --depth=1."""
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)
        mock_run.return_value = subprocess.CompletedProcess(
            [], 0, stdout="", stderr=""
        )
        rtpm.update_plugin("repo", plugin_dir)

        git_calls = [call[0][0] for call in mock_run.call_args_list]
        sub_call = git_calls[3]  # symbolic-ref, fetch, reset, submodule
        assert "submodule" in sub_call
        assert "--depth=1" in sub_call


class TestSourcePlugins:
    def test_runs_tmux_files(self, tmp_path):
        plugin_dir = tmp_path / "plugins"
        pdir = plugin_dir / "my-plugin"
        pdir.mkdir(parents=True)

        tmux_file = pdir / "my-plugin.tmux"
        tmux_file.write_text("#!/bin/bash\necho sourced\n")
        tmux_file.chmod(tmux_file.stat().st_mode | stat.S_IEXEC)

        spec = PluginSpec(name="my-plugin", repo="user/my-plugin")
        # Should not raise
        rtpm.source_plugins([spec], plugin_dir)

    def test_skips_missing_dirs(self, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        spec = PluginSpec(name="nonexistent", repo="user/nonexistent")
        # Should not raise
        rtpm.source_plugins([spec], plugin_dir)
