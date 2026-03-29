"""Tests for CLI commands: install, update, clean, list, menu, and CLI parser."""

import argparse
import subprocess
from unittest import mock

from . import conftest

rtpm = conftest.rtpm
PluginSpec = rtpm.PluginSpec


class TestCmdInstall:
    @mock.patch.object(rtpm, "get_plugin_dir")
    @mock.patch.object(rtpm, "clone_plugin", return_value=True)
    def test_install_missing(self, mock_clone, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/my-plugin'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_install(args)
        assert ret == 0
        mock_clone.assert_called_once()

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_install_already_exists(self, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "my-plugin").mkdir(parents=True)
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/my-plugin'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_install(args)
        assert ret == 0

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_install_skips_tpm(self, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'tmux-plugins/tpm'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_install(args)
        assert ret == 0

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_install_skips_rtpm(self, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/rtpm'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_install(args)
        assert ret == 0

    @mock.patch.object(rtpm, "get_plugin_dir")
    @mock.patch.object(rtpm, "clone_plugin", return_value=False)
    def test_install_failure_returns_nonzero(self, mock_clone, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/broken-plugin'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_install(args)
        assert ret == 1


class TestCmdClean:
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_clean_orphaned(self, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "wanted").mkdir(parents=True)
        (plugin_dir / "orphaned").mkdir(parents=True)
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/wanted'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_clean(args)
        assert ret == 0
        assert (plugin_dir / "wanted").exists()
        assert not (plugin_dir / "orphaned").exists()

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_clean_preserves_rtpm(self, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "rtpm").mkdir(parents=True)
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("")

        args = argparse.Namespace(config=str(conf))
        rtpm.cmd_clean(args)
        assert (plugin_dir / "rtpm").exists()

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_clean_preserves_tpm(self, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "tpm").mkdir(parents=True)
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("")

        args = argparse.Namespace(config=str(conf))
        rtpm.cmd_clean(args)
        assert (plugin_dir / "tpm").exists()

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_clean_nonexistent_dir(self, mock_dir, tmp_path):
        mock_dir.return_value = tmp_path / "nonexistent"

        args = argparse.Namespace(config=str(tmp_path / "tmux.conf"))
        (tmp_path / "tmux.conf").write_text("")
        ret = rtpm.cmd_clean(args)
        assert ret == 0


class TestCmdList:
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_list_output(self, mock_dir, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "installed").mkdir(parents=True)
        (plugin_dir / "orphan").mkdir(parents=True)
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text(
            "set -g @plugin 'user/installed'\n"
            "set -g @plugin 'user/missing'\n"
        )

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_list(args)
        assert ret == 0

        out = capsys.readouterr().out
        # markers have ANSI color codes inside, check text fragments
        assert "installed" in out
        assert "missing" in out
        assert "orphan" in out
        assert "Orphaned" in out

    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_list_with_branch(self, mock_dir, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/repo#develop'\n")

        args = argparse.Namespace(config=str(conf))
        rtpm.cmd_list(args)

        out = capsys.readouterr().out
        assert "branch: develop" in out


class TestCmdUpdate:
    @mock.patch.object(rtpm, "get_plugin_dir")
    @mock.patch.object(rtpm, "update_plugin", return_value=(True, "Already up to date."))
    def test_update_all(self, mock_update, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text(
            "set -g @plugin 'user/plugin-a'\n"
            "set -g @plugin 'user/plugin-b'\n"
        )

        args = argparse.Namespace(config=str(conf), plugins=["all"])
        ret = rtpm.cmd_update(args)
        assert ret == 0
        assert mock_update.call_count == 2

    @mock.patch.object(rtpm, "get_plugin_dir")
    @mock.patch.object(rtpm, "update_plugin", return_value=(True, "Already up to date."))
    def test_update_specific(self, mock_update, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/plugin-a'\n")

        args = argparse.Namespace(config=str(conf), plugins=["plugin-a"])
        ret = rtpm.cmd_update(args)
        assert ret == 0
        mock_update.assert_called_once_with("plugin-a", plugin_dir)

    @mock.patch.object(rtpm, "get_plugin_dir")
    @mock.patch.object(rtpm, "update_plugin", return_value=(False, "error"))
    def test_update_failure(self, mock_update, mock_dir, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/broken'\n")

        args = argparse.Namespace(config=str(conf), plugins=["all"])
        ret = rtpm.cmd_update(args)
        assert ret == 1


class TestMenuHelpers:
    """Test _menu_do_install and _menu_do_update."""

    @mock.patch.object(rtpm, "clone_plugin", return_value=True)
    def test_install_skips_existing(self, mock_clone, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "already").mkdir(parents=True)

        specs = [
            rtpm.PluginSpec(name="already", repo="user/already"),
            rtpm.PluginSpec(name="new-one", repo="user/new-one"),
        ]
        rtpm._menu_do_install(specs, plugin_dir)

        out = capsys.readouterr().out
        assert "already" not in out  # silently skipped
        assert "new-one" in out
        mock_clone.assert_called_once()

    def test_install_nothing_to_do(self, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "repo").mkdir(parents=True)

        specs = [rtpm.PluginSpec(name="repo", repo="user/repo")]
        rtpm._menu_do_install(specs, plugin_dir)

        out = capsys.readouterr().out
        assert "already installed" in out.lower()

    @mock.patch.object(rtpm, "_read_line", return_value="a")
    @mock.patch.object(rtpm, "update_plugin", return_value=(True, "ok"))
    def test_update_select_all(self, mock_update, mock_line, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "p1").mkdir(parents=True)
        (plugin_dir / "p2").mkdir(parents=True)

        specs = [
            rtpm.PluginSpec(name="p1", repo="user/p1"),
            rtpm.PluginSpec(name="p2", repo="user/p2"),
        ]
        rtpm._menu_do_update(specs, plugin_dir)
        assert mock_update.call_count == 2

    @mock.patch.object(rtpm, "_read_line", return_value="1")
    @mock.patch.object(rtpm, "update_plugin", return_value=(True, "ok"))
    def test_update_select_one(self, mock_update, mock_line, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "p1").mkdir(parents=True)
        (plugin_dir / "p2").mkdir(parents=True)

        specs = [
            rtpm.PluginSpec(name="p1", repo="user/p1"),
            rtpm.PluginSpec(name="p2", repo="user/p2"),
        ]
        rtpm._menu_do_update(specs, plugin_dir)
        mock_update.assert_called_once_with("p1", plugin_dir)

    @mock.patch.object(rtpm, "_read_line", return_value="1 2")
    @mock.patch.object(rtpm, "update_plugin", return_value=(True, "ok"))
    def test_update_select_multiple(self, mock_update, mock_line, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "p1").mkdir(parents=True)
        (plugin_dir / "p2").mkdir(parents=True)

        specs = [
            rtpm.PluginSpec(name="p1", repo="user/p1"),
            rtpm.PluginSpec(name="p2", repo="user/p2"),
        ]
        rtpm._menu_do_update(specs, plugin_dir)
        assert mock_update.call_count == 2

    @mock.patch.object(rtpm, "_read_line", return_value="q")
    @mock.patch.object(rtpm, "update_plugin")
    def test_update_cancel(self, mock_update, mock_line, tmp_path):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "p1").mkdir(parents=True)

        specs = [rtpm.PluginSpec(name="p1", repo="user/p1")]
        rtpm._menu_do_update(specs, plugin_dir)
        mock_update.assert_not_called()

    @mock.patch.object(rtpm, "_read_line", return_value="xyz")
    @mock.patch.object(rtpm, "update_plugin")
    def test_update_invalid_selection(self, mock_update, mock_line, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "p1").mkdir(parents=True)

        specs = [rtpm.PluginSpec(name="p1", repo="user/p1")]
        rtpm._menu_do_update(specs, plugin_dir)
        mock_update.assert_not_called()
        assert "No valid" in capsys.readouterr().out

    def test_update_no_installed_plugins(self, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()

        specs = [rtpm.PluginSpec(name="missing", repo="user/missing")]
        rtpm._menu_do_update(specs, plugin_dir)
        assert "No installed" in capsys.readouterr().out


class TestCmdMenu:
    """Test the interactive menu command (mocked keypress)."""

    @mock.patch.object(rtpm, "_read_key", return_value="q")
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_menu_quit(self, mock_dir, mock_key, tmp_path, capsys):
        plugin_dir = tmp_path / "plugins"
        (plugin_dir / "my-plugin").mkdir(parents=True)
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/my-plugin'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_menu(args)
        assert ret == 0

        out = capsys.readouterr().out
        assert "my-plugin" in out
        assert "[i]" in out
        assert "[u]" in out
        assert "[c]" in out
        assert "[q]" in out

    @mock.patch.object(rtpm, "_read_key", side_effect=["i", "q"])
    @mock.patch.object(rtpm, "_wait_key")
    @mock.patch.object(rtpm, "_menu_do_install")
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_menu_install_then_quit(self, mock_dir, mock_install, mock_wait, mock_key, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/repo'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_menu(args)
        assert ret == 0
        mock_install.assert_called_once()

    @mock.patch.object(rtpm, "_read_key", side_effect=["u", "q"])
    @mock.patch.object(rtpm, "_wait_key")
    @mock.patch.object(rtpm, "_menu_do_update")
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_menu_update_then_quit(self, mock_dir, mock_update, mock_wait, mock_key, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/repo'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_menu(args)
        assert ret == 0
        mock_update.assert_called_once()

    @mock.patch.object(rtpm, "_read_key", side_effect=["c", "q"])
    @mock.patch.object(rtpm, "_wait_key")
    @mock.patch.object(rtpm, "cmd_clean", return_value=0)
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_menu_clean_then_quit(self, mock_dir, mock_clean, mock_wait, mock_key, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("set -g @plugin 'user/repo'\n")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_menu(args)
        assert ret == 0
        mock_clean.assert_called_once()

    @mock.patch.object(rtpm, "_read_key", return_value="\x1b")
    @mock.patch.object(rtpm, "get_plugin_dir")
    def test_menu_esc_quits(self, mock_dir, mock_key, tmp_path):
        plugin_dir = tmp_path / "plugins"
        plugin_dir.mkdir()
        mock_dir.return_value = plugin_dir

        conf = tmp_path / "tmux.conf"
        conf.write_text("")

        args = argparse.Namespace(config=str(conf))
        ret = rtpm.cmd_menu(args)
        assert ret == 0


class TestGetTmuxOption:
    @mock.patch("subprocess.run")
    def test_returns_configured_value(self, mock_run):
        mock_run.return_value = subprocess.CompletedProcess(
            [], 0, stdout="P\n", stderr=""
        )
        assert rtpm._get_tmux_option("@rtpm-key", "M-i") == "P"

    @mock.patch("subprocess.run")
    def test_returns_default_when_empty(self, mock_run):
        mock_run.return_value = subprocess.CompletedProcess(
            [], 0, stdout="", stderr=""
        )
        assert rtpm._get_tmux_option("@rtpm-key", "M-i") == "M-i"

    @mock.patch("subprocess.run", side_effect=FileNotFoundError)
    def test_returns_default_when_tmux_missing(self, mock_run):
        assert rtpm._get_tmux_option("@rtpm-key", "M-i") == "M-i"


class TestRegisterKeybindings:
    @mock.patch.object(rtpm, "_get_tmux_option", return_value="M-i")
    @mock.patch("subprocess.run")
    def test_binds_default_key(self, mock_run, mock_opt):
        mock_run.return_value = subprocess.CompletedProcess([], 0)
        rtpm._register_keybindings()

        call_args = mock_run.call_args[0][0]
        assert "bind-key" in call_args
        assert "M-i" in call_args
        assert "display-popup" in call_args
        assert "menu" in call_args[-1]

    @mock.patch.object(rtpm, "_get_tmux_option", return_value="P")
    @mock.patch("subprocess.run")
    def test_binds_custom_key(self, mock_run, mock_opt):
        mock_run.return_value = subprocess.CompletedProcess([], 0)
        rtpm._register_keybindings()

        call_args = mock_run.call_args[0][0]
        assert "P" in call_args


class TestCLI:
    def test_no_command_shows_help(self, capsys):
        ret = rtpm.main([])
        assert ret == 0

    def test_install_command(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["install"])
        assert args.command == "install"

    def test_update_all(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["update"])
        assert args.command == "update"
        assert args.plugins == ["all"]

    def test_update_specific(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["update", "tmux-resurrect", "tmux-osinfo"])
        assert args.plugins == ["tmux-resurrect", "tmux-osinfo"]

    def test_config_flag(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["-c", "/path/to/tmux.conf", "list"])
        assert args.config == "/path/to/tmux.conf"
        assert args.command == "list"

    def test_source_command(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["source"])
        assert args.command == "source"

    def test_clean_command(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["clean"])
        assert args.command == "clean"

    def test_menu_command(self):
        parser = rtpm.build_parser()
        args = parser.parse_args(["menu"])
        assert args.command == "menu"
