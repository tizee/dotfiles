local wezterm = require 'wezterm';

return {
  -- set to false to disable the tab bar completely
  enable_tab_bar = false,
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
    }
    }),
  color_scheme = "Afterglow",
}
