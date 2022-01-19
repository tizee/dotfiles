-- generic mode
local mode_comp = {}

-- keycode: A string containing the name of a keyboard key (as found in hs.keycodes.map ), or a raw keycode number
-- modeMappings: mapping configuration table
-- opTable: table of operations
mode_comp.new = function(modeName,keycode,modeMappings,opTable)
  local hotkey_modal = hs.hotkey.modal.new({}, keycode)
  local modifiers = modeMappings.modifiers
  local showHelp  = modeMappings.showHelp
  local trigger   = modeMappings.trigger
  local mappings  = modeMappings.mappings
  hotkey_modal.entered = function()
    hotkey_modal.statusMessage:show()
  end
  hotkey_modal.exited = function()
    hotkey_modal.statusMessage:hide()
  end
  -- Bind the given key to call the given function and exit WindowLayout mode
  function hotkey_modal.bindWithAutomaticExit(mode, modifiers, key, fn)
    mode:bind(modifiers, key, function()
      mode:exit()
      fn()
    end)
  end

  local function getModifiersStr(modifiers)
    local modMap = { shift = '⇧', ctrl = '⌃', alt = '⌥', cmd = '⌘' }
    local retVal = ''

    for i, v in ipairs(modifiers) do
      retVal = retVal .. modMap[v]
    end

    return retVal
  end

  local msgStr = getModifiersStr(modifiers)
  msgStr = modeName .. ' (' .. msgStr .. (string.len(msgStr) > 0 and '+' or '') .. trigger .. ')'

  for i, mapping in ipairs(mappings) do
    local modifiers, trigger, opName = table.unpack(mapping)
    local hotKeyStr = getModifiersStr(modifiers)

    if showHelp == true then
      if string.len(hotKeyStr) > 0 then
        msgStr = msgStr .. (string.format('\n%10s+%s => %s', hotKeyStr, trigger, opName))
      else
        msgStr = msgStr .. (string.format('\n%11s => %s', trigger, opName))
      end
    end

    hotkey_modal:bindWithAutomaticExit(modifiers, trigger, function()
      opTable[opName]()
    end)
  end

  local statusmessage = require('status-message')
  hotkey_modal.statusMessage = statusmessage.new(msgStr)

  -- Use modifiers+trigger to toggle WindowLayout Mode
  hs.hotkey.bind(modifiers, trigger, function()
    hotkey_modal:enter()
  end)
  hotkey_modal:bind(modifiers, trigger, function()
    hotkey_modal:exit()
  end)

  return hotkey_modal
end


return mode_comp
