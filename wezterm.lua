local wezterm = require 'wezterm';

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = utf8.char(0xe0b0)

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local edge_background = "#0b0022"
  local background = "#1b1032"
  local foreground = "#808080"

  if tab.is_active then
    background = "#2b2042"
    foreground = "#c0c0c0"
  elseif hover then
    background = "#3b3052"
    foreground = "#909090"
  end

  local edge_foreground = background

  -- ensure that the titles fit in the available space,
  -- and that we have room for the edges
  local title = wezterm.truncate_right(tab.active_pane.title, max_width-2)

  return {
    {Background={Color=edge_background}},
    {Foreground={Color=edge_foreground}},
    {Text=SOLID_LEFT_ARROW},
    {Background={Color=background}},
    {Foreground={Color=foreground}},
    {Text=title},
    {Background={Color=edge_background}},
    {Foreground={Color=edge_foreground}},
    {Text=SOLID_RIGHT_ARROW},
  }
end)

return {
  -- general settings <-
  check_for_updates = true,
  exit_behavior="CloseOnCleanExit", -- only exit with a successful status
  default_cursor_style = "SteadyUnderline",
  -- http://www.leonerd.org.uk/hacks/fixterms/
  enable_csi_u_key_encoding = true,
  -- ->
  -- window settings <-
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  window_decorations = "RESIZE",
  window_background_opacity = 0.95,
  -- macos
  native_macos_fullscreen_mode = true,
  -- ->
  -- tab bar settings <-
  -- set to false to disable the tab bar completely
  enable_tab_bar = true,
  use_fancy_tab_bar = false,
  hide_tab_bar_if_only_one_tab = true,
  -- ->
  -- font settings <-
  font_size = 16,
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
    "SimHei" -- for Chinese or Japanese
    }),
  -- ->
  -- hotkeys <-
  -- tmux uses CTRL-A
  leader = { key="a", mods="CMD", timeout_milliseconds=1002 },
  keys = {
     -- Send "CTRL-A" to the terminal when pressing LEADER-a for tmux
    {key="a", mods="LEADER", action=wezterm.action{SendString="\x01"}},
    -- h,j,k,l move between panes
    {key="h", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="j", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="k", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="l", mods="LEADER", action=wezterm.action{ActivatePaneDirection="Right"}},
    -- vertical split direction
    {key="s", mods="LEADER", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
    -- horizontal split direction
    {key="d", mods="LEADER", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
    -- close current pane
    {key="x", mods="LEADER", action=wezterm.action{CloseCurrentPane={confirm=true}}},
    -- zoom state
    {key="z", mods="LEADER", action="TogglePaneZoomState"},
    -- fullscreen
    {key="f", mods="LEADER", action="ToggleFullScreen"},
    -- debug
    {key="p", mods="LEADER", action="ShowDebugOverlay"},
    -- copy mode
    {key="c", mods="LEADER", action="ActivateCopyMode"},
    -- quick select mode - git hash, url etc.
    {key="q", mods="LEADER", action="QuickSelect"},
    -- reload configuration
    {key="r", mods="LEADER", action="ReloadConfiguration"},
  },
  -- ->
  -- IME
  -- https://github.com/wez/wezterm/pull/1096
  send_composed_key_when_left_alt_is_pressed=false,
  send_composed_key_when_right_alt_is_pressed=true,
  use_ime = true,
  use_dead_keys= false, -- prevent combination 
  -- debug by launching wezterm in other terminal
  debug_key_events = true,
  color_scheme = "Dracula",
  -- font shaping that enables ligatures
  -- see https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist
  harfbuzz_features = {"calt=1", "clig=1", "liga=1"},
}

-- vim:foldmarker=<-,-> foldmethod=marker
