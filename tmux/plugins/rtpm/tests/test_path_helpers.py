"""Tests for path expansion, find_tmux_conf, get_plugin_dir."""

from pathlib import Path

from . import conftest

rtpm = conftest.rtpm
expand_path = rtpm.expand_path
find_tmux_conf = rtpm.find_tmux_conf


class TestExpandPath:
    def test_tilde(self):
        result = expand_path("~/foo")
        assert result == Path.home() / "foo"

    def test_dollar_home(self):
        result = expand_path("$HOME/bar")
        assert result == Path.home() / "bar"

    def test_absolute(self):
        result = expand_path("/usr/local/bin")
        assert result == Path("/usr/local/bin")

    def test_no_expansion_needed(self):
        result = expand_path("/tmp/test")
        assert result == Path("/tmp/test")


class TestFindTmuxConf:
    def test_xdg_first(self, tmp_path, monkeypatch):
        xdg = tmp_path / "xdg"
        (xdg / "tmux").mkdir(parents=True)
        (xdg / "tmux" / "tmux.conf").write_text("# test")
        monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg))
        assert find_tmux_conf() == xdg / "tmux" / "tmux.conf"

    def test_fallback_to_home(self, tmp_path, monkeypatch):
        monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "nonexistent"))
        monkeypatch.setattr(rtpm.Path, "home", lambda: tmp_path)
        result = find_tmux_conf()
        assert result == tmp_path / ".tmux.conf"


class TestGetPluginDir:
    def test_env_var(self, monkeypatch):
        monkeypatch.setenv("TMUX_PLUGIN_MANAGER_PATH", "/custom/plugins")
        result = rtpm.get_plugin_dir()
        assert result == Path("/custom/plugins")

    def test_env_var_with_tilde(self, monkeypatch):
        monkeypatch.setenv("TMUX_PLUGIN_MANAGER_PATH", "~/my-plugins")
        result = rtpm.get_plugin_dir()
        assert result == Path.home() / "my-plugins"
