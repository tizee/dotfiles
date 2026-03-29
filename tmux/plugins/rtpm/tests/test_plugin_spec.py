"""Tests for PluginSpec and parse_plugin_spec."""

from . import conftest

rtpm = conftest.rtpm
PluginSpec = rtpm.PluginSpec
parse_plugin_spec = rtpm.parse_plugin_spec


class TestPluginSpec:
    def test_github_shorthand(self):
        s = PluginSpec(name="tmux-resurrect", repo="tmux-plugins/tmux-resurrect")
        assert s.clone_url == "https://git::@github.com/tmux-plugins/tmux-resurrect"

    def test_full_https_url(self):
        url = "https://github.com/user/repo.git"
        s = PluginSpec(name="repo", repo=url)
        assert s.clone_url == url

    def test_git_ssh_url(self):
        url = "git@github.com:user/repo.git"
        s = PluginSpec(name="repo", repo=url)
        assert s.clone_url == url


class TestParsePluginSpec:
    def test_simple(self):
        s = parse_plugin_spec("tmux-plugins/tmux-resurrect")
        assert s.name == "tmux-resurrect"
        assert s.repo == "tmux-plugins/tmux-resurrect"
        assert s.branch is None

    def test_with_branch(self):
        s = parse_plugin_spec("user/repo#develop")
        assert s.name == "repo"
        assert s.repo == "user/repo"
        assert s.branch == "develop"

    def test_full_url(self):
        s = parse_plugin_spec("https://github.com/user/my-plugin.git")
        assert s.name == "my-plugin"
        assert s.repo == "https://github.com/user/my-plugin.git"
        assert s.branch is None

    def test_full_url_with_branch(self):
        s = parse_plugin_spec("https://github.com/user/plugin.git#v2")
        assert s.name == "plugin"
        assert s.branch == "v2"

    def test_trailing_slash(self):
        s = parse_plugin_spec("user/repo/")
        assert s.name == "repo"

    def test_tpm_self(self):
        s = parse_plugin_spec("tmux-plugins/tpm")
        assert s.name == "tpm"
