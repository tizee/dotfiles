local wezterm = require 'wezterm';

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = utf8.char(0xe0b0)

-- The powerline < symbol
local LEFT_ARROW = utf8.char(0xe0b3);

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local background = "#1b1032"
  local foreground = "#808080"

  if tab.is_active then
    background = "#3c1361"
    foreground = "#c0c0c0"
  elseif hover then
    background = "#3b3052"
    foreground = "#909090"
  end

  -- ensure that the titles fit in the available space,
  -- and that we have room for the edges
  local title = wezterm.truncate_right(tab.active_pane.title, max_width)

  return {
    {Background={Color=background}},
    {Foreground={Color=foreground}},
    {Text=" "..tab.tab_index..":"..tab.active_pane.pane_id.." "..title.." "},
  }
end)

wezterm.on("update-right-status", function(window, pane)
  -- Each element holds the text for a cell in a "powerline" style << fade
  local cells = {};
  local leader = ""
  if window:leader_is_active() then
    leader = "<LEADER>"
    table.insert(cells,leader)
  end
  -- Figure out the cwd and host of the current pane.
  -- This will pick up the hostname for the remote host if your
  -- shell is using OSC 7 on the remote host.
  local cwd_uri = pane:get_current_working_dir()
  if cwd_uri then
    cwd_uri = cwd_uri:sub(8);
    local slash = cwd_uri:find("/")
    local cwd = ""
    local hostname = ""
    if slash then
      hostname = cwd_uri:sub(1, slash-1)
      -- Remove the domain name portion of the hostname
      local dot = hostname:find("[.]")
      if dot then
        hostname = hostname:sub(1, dot-1)
      end
      print(hostname)
      -- and extract the cwd from the uri
      cwd = cwd_uri:sub(slash)

      table.insert(cells, cwd);
      if string.len(hostname) > 0 then
        table.insert(cells, hostname);
      end
    end
  end

  local date = wezterm.strftime("%a %b %-d %H:%M");
  table.insert(cells, date);

  -- An entry for each battery (typically 0 or 1 battery)
  -- for _, b in ipairs(wezterm.battery_info()) do
    -- table.insert(cells, string.format("%.0f%%", b.state_of_charge * 100))
  -- end

  -- Color palette for the backgrounds of each cell
  local colors = {
    "#3c1361",
    "#52307c",
    "#663a82",
    "#7c5295",
    "#b491c8",
  };

  -- Foreground color for the text across the fade
  local text_fg = "#c0c0c0";

  -- The elements to be formatted
  local elements = {};
  -- How many cells have been formatted
  local num_cells = 0;

  -- Translate a cell into elements
function push(text, is_last)
    local cell_no = num_cells + 1
    table.insert(elements, {Foreground={Color=text_fg}})
    table.insert(elements, {Background={Color=colors[cell_no]}})
    table.insert(elements, {Text=" "..text.." "})
    if not is_last then
      table.insert(elements, {Foreground={Color=colors[cell_no+1]}})
      table.insert(elements, {Text=SOLID_LEFT_ARROW})
    end
    num_cells = num_cells + 1
  end

  while #cells > 0 do
    local cell = table.remove(cells, 1)
    push(cell, #cells == 0)
  end

  window:set_right_status(wezterm.format(elements));
end);

return {
  -- general settings <-
  term = "xterm-256color",
  hyperlink_rules = {
    -- Linkify things that look like URLs and the host has a TLD name.
    -- Compiled-in default. Used if you don't specify any hyperlink_rules.
    {
      regex = "\\b\\w+://[\\w.-]+\\.[a-z]{2,15}\\S*\\b",
      format = "$0",
    },

    -- linkify email addresses
    -- Compiled-in default. Used if you don't specify any hyperlink_rules.
    {
      regex = [[\b\w+@[\w-]+(\.[\w-]+)+\b]],
      format = "mailto:$0",
    },

    -- file:// URI
    -- Compiled-in default. Used if you don't specify any hyperlink_rules.
    {
      regex = [[\bfile://\S*\b]],
      format = "$0",
    },

    -- Linkify things that look like URLs with numeric addresses as hosts.
    -- E.g. http://127.0.0.1:8000 for a local development server,
    -- or http://192.168.1.1 for the web interface of many routers.
    {
      regex = [[\b\w+://(?:[\d]{1,3}\.){3}[\d]{1,3}\S*\b]],
      format = "$0",
    },

    -- Make task numbers clickable
    -- The first matched regex group is captured in $1.
    {
      regex = [[\b[tT](\d+)\b]],
      format = "https://example.com/tasks/?t=$1",
    },

    -- Make username/project paths clickable. This implies paths like the following are for GitHub.
    -- ( "nvim-treesitter/nvim-treesitter" | wbthomason/packer.nvim | wez/wezterm | "wez/wezterm.git" )
    -- As long as a full URL hyperlink regex exists above this it should not match a full URL to
    -- GitHub or GitLab / BitBucket (i.e. https://gitlab.com/user/project.git is still a whole clickable URL)
    {
      regex = [[["]?([\w\d]{1}[-\w\d]+)(/){1}([-\w\d\.]+)["]?]],
      format = "https://www.github.com/$1/$3",
    }
  },
  -- drop files in macOS
  quote_dropped_files = "Posix",
  colors = {
      compose_cursor = "green",
      tab_bar = {
      -- The color of the strip that goes along the top of the window
      -- (does not apply when fancy tab bar is in use)
      background = "#0b0022",

      -- The active tab is the one that has focus in the window
      active_tab = {
        -- The color of the background area for the tab
        bg_color = "#2b2042",
        -- The color of the text for the tab
        fg_color = "#c0c0c0",

        -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
        -- label shown for this tab.
        -- The default is "Normal"
        intensity = "Normal",

        -- Specify whether you want "None", "Single" or "Double" underline for
        -- label shown for this tab.
        -- The default is "None"
        underline = "None",

        -- Specify whether you want the text to be italic (true) or not (false)
        -- for this tab.  The default is false.
        italic = false,

        -- Specify whether you want the text to be rendered with strikethrough (true)
        -- or not for this tab.  The default is false.
        strikethrough = false,
      },

      -- Inactive tabs are the tabs that do not have focus
      inactive_tab = {
        bg_color = "#1b1032",
        fg_color = "#808080",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab`.
      },

      -- You can configure some alternate styling when the mouse pointer
      -- moves over inactive tabs
      inactive_tab_hover = {
        bg_color = "#3b3052",
        fg_color = "#909090",
        italic = true,

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab_hover`.
      },

      -- The new tab button that let you create new tabs
      new_tab = {
        bg_color = "#1b1032",
        fg_color = "#808080",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `new_tab`.
      },

      -- You can configure some alternate styling when the mouse pointer
      -- moves over the new tab button
      new_tab_hover = {
        bg_color = "#3b3052",
        fg_color = "#909090",
        italic = true,

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `new_tab_hover`.
      }
    }
  },
  check_for_updates = true,
  exit_behavior="CloseOnCleanExit", -- only exit with a successful status
  default_cursor_style = "SteadyUnderline",
  -- http://www.leonerd.org.uk/hacks/fixterms/
  enable_csi_u_key_encoding = true,
  force_reverse_video_cursor = true,
  -- ->
-- mouse bindings <-
  mouse_bindings = {
    -- Bind 'Up' event of CTRL-Click to open hyperlinks
    {
      event={Up={streak=1, button="Left"}},
      mods="CTRL",
      action="OpenLinkAtMouseCursor",
    },
    -- Disable the 'Down' event of CTRL-Click to avoid weird program behaviors
    {
      event={Down={streak=1, button="Left"}},
      mods="CTRL",
      action="Nop",
    },
  },
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
  font_size = 20,
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
    {
      family="LXGW WenKai Mono", -- for Chinese or Japanese
      weight="Regular"
    }
    }),
  -- ->
  -- hotkeys <-
  -- tmux uses CTRL-A
  leader = { key="a", mods="CMD", timeout_milliseconds=1004 },
  keys = {
    -- {key="d", mods="CTRL", action="Nop"},
    {key="d", mods="LEADER", action=wezterm.action{SendString="\x01"}},
     -- Send "CTRL-A" to the terminal when pressing LEADER-a for tmux
    {key="a", mods="LEADER", action=wezterm.action{SendString="\x01"}},
    -- pane navigation
    {key="LeftArrow",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="h",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="RightArrow",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Right"}},
    {key="l",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Right"}},
    {key="UpArrow",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="k",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="DownArrow",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="j",mods="LEADER", action=wezterm.action{ActivatePaneDirection="Down"}},
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
    -- wezterm auto re-load when config changes
    -- {key="r", mods="LEADER", action="ReloadConfiguration"},
    {key="r", mods="LEADER", action=wezterm.action{ ActivateKeyTable={
        name="resize_pane",
        one_shot=false,
        replace_current=true,
    }}},
    -- move tab
    {key="<", mods="LEADER", action=wezterm.action{MoveTabRelative=-1}},
    {key=">", mods="LEADER", action=wezterm.action{MoveTabRelative=1}},
    {key="{", mods="ALT", action=wezterm.action{ActivateTabRelative=-1}},
    {key="}", mods="ALT", action=wezterm.action{ActivateTabRelative=1}},
    {key="t", mods="LEADER", action="ShowTabNavigator"},
    -- scroll
    {key="1", mods="LEADER", action=wezterm.action{ScrollByPage=-0.5}},
    {key="2", mods="LEADER", action=wezterm.action{ScrollByPage=0.5}},
    -- Clears only the scrollback and leaves the viewport intact.
    -- This is the default behavior.
    -- {key="k", mods="LEADER", action=wezterm.action{ClearScrollback="ScrollbackOnly"}}
    -- Clears the scrollback and viewport leaving the prompt line the new first line.
    -- {key="K", mods="LEADER", action=wezterm.action{ClearScrollback="ScrollbackAndViewport"}}
    {key="w", mods="LEADER", action=wezterm.action{ClearScrollback="ScrollbackOnly"}},
    {
    key = 'u',
    mods = 'LEADER',
    action = wezterm.action.CharSelect {
      copy_on_select = true,
      copy_to = 'ClipboardAndPrimarySelection',}},

  },
  -- save memory
  -- lines pertained per tab
  scrollback_lines = 3500,
  key_tables = {
    -- Defines the keys that are active in our resize-pane mode.
    -- Since we're likely to want to make multiple adjustments,
    -- we made the activation one_shot=false. We therefore need
    -- to define a key assignment for getting out of this mode.
    -- 'resize_pane' here corresponds to the name="resize_pane" in
    -- the key assignments above.
    resize_pane = {
      {key="LeftArrow", action=wezterm.action{AdjustPaneSize={"Left", 1}}},
      {key="h", action=wezterm.action{AdjustPaneSize={"Left", 1}}},

      {key="RightArrow", action=wezterm.action{AdjustPaneSize={"Right", 1}}},
      {key="l", action=wezterm.action{AdjustPaneSize={"Right", 1}}},

      {key="UpArrow", action=wezterm.action{AdjustPaneSize={"Up", 1}}},
      {key="k", action=wezterm.action{AdjustPaneSize={"Up", 1}}},

      {key="DownArrow", action=wezterm.action{AdjustPaneSize={"Down", 1}}},
      {key="j", action=wezterm.action{AdjustPaneSize={"Down", 1}}},

      -- Cancel the mode by pressing escape
      {key="Escape", action="PopKeyTable"},

    },
  },
  -- ->
  -- IME <-
  -- https://github.com/wez/wezterm/pull/1096
  send_composed_key_when_left_alt_is_pressed=false,
  send_composed_key_when_right_alt_is_pressed=true,
  use_ime = true,
  -- ->
  -- key assignment debug <-
  use_dead_keys= false, -- prevent combination
  -- debug by launching wezterm in other terminal
  debug_key_events = true,
  -- ->
  color_scheme = "Dracula",
  -- font shaping that enables ligatures
  -- see https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist
  harfbuzz_features = {"calt=1", "clig=1", "liga=1"},
}

-- vim:foldmarker=<-,-> foldmethod=marker
