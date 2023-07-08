-- Hammerspoon based mode component
local log = hs.logger.new('mode','debug')

-- generic mode component
local mode_comp = {}

-- helper
local function getModifiersStr(modifiers)
  local modMap = { shift = '⇧', ctrl = '⌃', alt = '⌥', cmd = '⌘' }
  local modStr = ''

  for _, v in ipairs(modifiers) do
    modStr = modStr .. modMap[v]
  end

  return modStr
end

-- modeName: mode name
-- keycode: global hotkey for current mode
-- modeConfig: configuration for mode
-- mappings: key bindings
-- opTable: actions for key bindings
local function newModal(modeName,keycode,modeConfig,mappings,opTable)
  local hotkey_modal = hs.hotkey.modal.new({}, keycode)
  local showHelp  = modeConfig.showHelp
  local modifiers = modeConfig.modifiers
  local trigger   = modeConfig.trigger

  hotkey_modal.entered = function()
    hotkey_modal.statusMessage:show()
  end
  hotkey_modal.exited = function()
    hotkey_modal.statusMessage:hide()
  end

  -- Bind the given key to call the given function and exit mode automatically
  function hotkey_modal.bindWithAutomaticExit(mode, modifier, key, fn)
    mode:bind(modifier, key, function()
      mode:exit()
      fn()
    end)
  end


  -- modal text
  local msgStr = getModifiersStr(modifiers)
  msgStr = modeName .. ' (' .. msgStr .. (string.len(msgStr) > 0 and '+' or '') .. trigger .. ')'

  for _, mapping in ipairs(mappings) do
    local modifier, action_trigger, opName = table.unpack(mapping)
    local hotKeyStr = getModifiersStr(modifier)

    if showHelp == true then
      if string.len(hotKeyStr) > 0 then
        msgStr = msgStr .. (string.format('\n%10s+%s => %s', hotKeyStr, action_trigger, opName))
      else
        msgStr = msgStr .. (string.format('\n%11s => %s', action_trigger, opName))
      end
    end
   local statusmessage = require('status-message')
   hotkey_modal.statusMessage = statusmessage.new(msgStr)

   -- bind action
   hotkey_modal:bindWithAutomaticExit(modifier, action_trigger, function()
      opTable[opName]()
    end)
  end
  return hotkey_modal
end

-- create the modal component each time
mode_comp.new_reactive = function(modeName, keycode, modeConfig, update_mappings,update_actions)
  -- static fields
  local modifiers = modeConfig.modifiers
  local trigger   = modeConfig.trigger

  local hotkey_modal = {}

  -- Use modifiers+trigger to toggle Mode
  hs.hotkey.bind(modifiers, trigger, function()
    local mappings  = update_mappings()
    local opTable = update_actions()
    hotkey_modal = newModal(modeName, keycode, modeConfig,mappings,opTable)
    hotkey_modal:bind(modifiers, trigger, function()
      hotkey_modal:exit()
    end)
    hotkey_modal:enter()
  end)
  return nil
end

-- static mode component
-- modeName: mode name
-- keycode: A string containing the name of a keyboard key (as found in hs.keycodes.map ), or a raw keycode number, usually this is an non-op keycode
-- modeMappings: mapping configuration table
-- opTable: table of operations
mode_comp.new = function(modeName,keycode,modeConfig,opTable)
  local modifiers = modeConfig.modifiers
  local trigger   = modeConfig.trigger
  local mappings  = modeConfig.mappings
  local hotkey_modal = newModal(modeName,keycode,modeConfig,mappings,opTable)

  -- Use modifiers+trigger to toggle Mode
  hs.hotkey.bind(modifiers, trigger, function()
    hotkey_modal:enter()
  end)
  hotkey_modal:bind(modifiers, trigger, function()
    hotkey_modal:exit()
  end)
  return nil
end

return mode_comp
