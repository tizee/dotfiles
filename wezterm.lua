local wezterm = require 'wezterm';

return {
  -- general settings <-
  check_for_updates = true,
  -- ->
  -- window settings <-
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  -- ->
  -- tab bar settings <-
  -- set to false to disable the tab bar completely
  enable_tab_bar = true,
  tab_bar = {
    active_bar = {
      italic = false,
    },
    inactive_bar = {
      italic = true,
    },
  },
  -- ->
  -- font settings <-
  font_size = 14,
  font = wezterm.font_with_fallback({
    {
      family="JetBrains Mono",
      weight="Regular",
      italic=false
    },
    {
      family="JetBrainsMono Nerd Font",
      weight="Bold"
    },
    "SimHei"
    }),
  -- ->
  -- hotkeys <-
  -- tmux uses CTRL-A
  leader = { key="a", mods="CMD", timeout_milliseconds=1001 },
  keys = {
    -- h,j,k,l move between panes
    {key="h", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="j", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="k", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="l", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Right"}},
    -- vertical split direction
    {key="s", mods="LEADER", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
    -- horizontal split direction
    {key="d", mods="LEADER", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
    -- zoom
    {key="z", mods="LEADER", action="TogglePaneZoomState"},
  },
  -- ->
  -- IME <-
  use_ime = false, -- conflict with leader mode, could submit a PR for this
  -- ->
  color_scheme = "Dracula",
}

-- vim:foldmarker=<-,-> foldmethod=marker
