"""Tests for config parsing: extract plugins, extract sources, parse_config."""

import textwrap
from pathlib import Path

from . import conftest

rtpm = conftest.rtpm
parse_config = rtpm.parse_config


class TestExtractPlugins:
    """Test the regex-based plugin line extraction."""

    def test_single_quotes(self):
        lines = ["set -g @plugin 'tmux-plugins/tmux-resurrect'"]
        assert rtpm._extract_plugins(lines) == ["tmux-plugins/tmux-resurrect"]

    def test_double_quotes(self):
        lines = ['set -g @plugin "user/repo"']
        assert rtpm._extract_plugins(lines) == ["user/repo"]

    def test_no_quotes(self):
        lines = ["set -g @plugin user/repo"]
        assert rtpm._extract_plugins(lines) == ["user/repo"]

    def test_set_option(self):
        lines = ["set-option -g @plugin 'user/repo'"]
        assert rtpm._extract_plugins(lines) == ["user/repo"]

    def test_leading_whitespace(self):
        lines = ["  set -g @plugin 'user/repo'"]
        assert rtpm._extract_plugins(lines) == ["user/repo"]

    def test_commented_out(self):
        lines = ["# set -g @plugin 'user/repo'"]
        assert rtpm._extract_plugins(lines) == []

    def test_multiple_plugins(self):
        lines = [
            "set -g @plugin 'tmux-plugins/tpm'",
            "set -g @plugin 'tmux-plugins/tmux-resurrect'",
            "set -g @plugin 'tizee/tmux-osinfo'",
        ]
        assert len(rtpm._extract_plugins(lines)) == 3

    def test_with_branch(self):
        lines = ["set -g @plugin 'user/repo#main'"]
        assert rtpm._extract_plugins(lines) == ["user/repo#main"]


class TestExtractSources:
    def test_source_file(self):
        lines = ["source-file ~/.config/tmux/tmux.osx.conf"]
        result = rtpm._extract_sources(lines)
        assert len(result) == 1
        assert "tmux.osx.conf" in str(result[0])

    def test_source_without_file(self):
        lines = ["source ~/.config/tmux/theme.conf"]
        result = rtpm._extract_sources(lines)
        assert len(result) == 1

    def test_source_with_quotes(self):
        lines = ["source-file '~/.config/tmux/theme.conf'"]
        result = rtpm._extract_sources(lines)
        assert len(result) == 1

    def test_source_with_q_flag(self):
        lines = ["source-file -q ~/.config/tmux/optional.conf"]
        result = rtpm._extract_sources(lines)
        assert len(result) == 1


class TestParseConfig:
    """Integration tests for full config parsing with sourced files."""

    def test_simple_config(self, tmp_path):
        conf = tmp_path / "tmux.conf"
        conf.write_text(textwrap.dedent("""\
            set -g @plugin 'tmux-plugins/tpm'
            set -g @plugin 'tmux-plugins/tmux-resurrect'
            set -g @plugin 'tizee/tmux-osinfo'
        """))
        specs = parse_config(conf)
        names = [s.name for s in specs]
        assert "tpm" in names
        assert "tmux-resurrect" in names
        assert "tmux-osinfo" in names

    def test_with_sourced_file(self, tmp_path):
        main_conf = tmp_path / "tmux.conf"
        osx_conf = tmp_path / "tmux.osx.conf"

        osx_conf.write_text(textwrap.dedent("""\
            set -g @plugin 'tmux-plugins/tmux-resurrect'
            set -g @plugin 'tizee/tmux-capture'
        """))
        main_conf.write_text(
            f"source-file {osx_conf}\n"
            "set -g @plugin 'tmux-plugins/tpm'\n"
        )

        specs = parse_config(main_conf)
        names = [s.name for s in specs]
        assert "tpm" in names
        assert "tmux-resurrect" in names
        assert "tmux-capture" in names

    def test_circular_source(self, tmp_path):
        """Circular source references should not loop forever."""
        a = tmp_path / "a.conf"
        b = tmp_path / "b.conf"
        a.write_text(
            f"source-file {b}\n"
            "set -g @plugin 'user/plugin-a'\n"
        )
        b.write_text(
            f"source-file {a}\n"
            "set -g @plugin 'user/plugin-b'\n"
        )
        specs = parse_config(a)
        names = [s.name for s in specs]
        assert "plugin-a" in names
        assert "plugin-b" in names

    def test_missing_source_file(self, tmp_path):
        conf = tmp_path / "tmux.conf"
        conf.write_text(
            "source-file /nonexistent/path.conf\n"
            "set -g @plugin 'user/repo'\n"
        )
        specs = parse_config(conf)
        assert len(specs) == 1
        assert specs[0].name == "repo"

    def test_commented_plugins_skipped(self, tmp_path):
        conf = tmp_path / "tmux.conf"
        conf.write_text(textwrap.dedent("""\
            set -g @plugin 'user/active'
            # set -g @plugin 'user/commented'
        """))
        specs = parse_config(conf)
        assert len(specs) == 1
        assert specs[0].name == "active"

    def test_real_config_format(self, tmp_path):
        """Test with a config resembling the actual tmux.osx.conf."""
        conf = tmp_path / "tmux.conf"
        conf.write_text(textwrap.dedent("""\
            # TPM
            set -g @plugin 'tmux-plugins/tpm'

            # plugins
            set -g @plugin 'tmux-plugins/tmux-resurrect'
            # set -g @plugin 'tmux-plugins/tmux-prefix-highlight'
            set -g @plugin 'tizee/tmux-osinfo'
            set -g @plugin 'tizee/tmux-capture'
            set -g @plugin 'tmux-plugin/tmux-continuum'

            # tmux-resurrect config
            set -g @resurrect-capture-pane-contents 'on'
            set -g @resurrect-save 'S'

            run '~/.config/tmux/plugins/tpm/tpm'
        """))
        specs = parse_config(conf)
        names = [s.name for s in specs]
        assert names == [
            "tpm",
            "tmux-resurrect",
            "tmux-osinfo",
            "tmux-capture",
            "tmux-continuum",
        ]

    def test_if_shell_with_source(self, tmp_path):
        """Test that source-file inside if-shell blocks is found."""
        osx_conf = tmp_path / "tmux.osx.conf"
        osx_conf.write_text("set -g @plugin 'user/osx-only'\n")

        conf = tmp_path / "tmux.conf"
        conf.write_text(textwrap.dedent(f"""\
            set -g @plugin 'user/base'

            if-shell 'test "$(uname)" = "Darwin"' {{
              source {osx_conf}
            }}
        """))
        specs = parse_config(conf)
        names = [s.name for s in specs]
        assert "base" in names
        assert "osx-only" in names
