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
    -- { {},         kn',      'nextScreen' },
    -- { {},         'right',  'moveOneScreenEast' },
    -- { {},         'left',   'moveOneScreenWest' },
  }
}
}
