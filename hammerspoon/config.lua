-- keymappings
return {
  -- whether auto-reload configuration when changed
  auto_reload = false,
  windowMappings = {
    modifiers = {'ctrl'},
    showHelp  = true,
    trigger   = 's',
    mappings  = {
      { {},         'return', 'maximize' },
      { {},         'space',  'centerWithFullHeight' },
      { {},         'h',      'left' },
      { {},         'j',      'down' },
      { {},         'k',      'up' },
      { {},         'l',      'right' },
      { {'shift'},  'h',      'left40' },
      { {'shift'},  'l',      'right60' },
      { {},         'i',      'upLeft' },
      { {},         'o',      'upRight' },
      { {},         ',',      'downLeft' },
      { {},         '.',      'downRight' },
      { {},         '1',      'win1140x700' },
      -- { {},         kn',      'nextScreen' },
      -- { {},         'right',  'moveOneScreenEast' },
      -- { {},         'left',   'moveOneScreenWest' },
    },
  },
  quickActionMappings = {
    modifiers = {'ctrl'},
    showHelp  = true,
    trigger   = 't',
    mappings  = {
      { {},         'q', 'quickNavigation'},
      { {},         'a', 'showAppInfo'},
      { {},         'd', 'toggleDarkMode'},
      { {},         'r',  'reloadConfig' },
      { {},         'y',  'pasteBoardYoutubeURL' },
      { {},         't',  'pasteBoardRemoveTrackers' },
      { {},         'h',      'Arc'},
      { {},         'j',      'Reeder'},
      { {},         'k',      'Obsidian'},
      { {},         'l',      'Telegram'},
      { {},         'i',      'Obsidian'},
    },
  }
}
