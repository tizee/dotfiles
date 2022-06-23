local mode_comp = require('mode')
local log = hs.logger.new('switch-window-mode','debug')

local function get_app_windows()
  -- find running app with given name
  local app = hs.window.focusedWindow():application()
  if not app then
    -- app = nil
    return {}
  end
  log.d(app:name())
  -- Spaces-aware windowfilters might experience a (sometimes significant) delay after every Space switch, since (due to OS X limitations) they must re-query for the list of all windows in the current Space every time.
  local wf = hs.window.filter.new(false):setAppFilter(app:name(),{visible=nil, currentSpace=nil})
  local windows = wf:getWindows()
  return windows
end

-- shortcuts to common Apps
local function switch_to(window)
  log.d("switch to",window)
  if window then
    window:focus()
  end
end

local mode_config = {
    modifiers = {'ctrl'},
    showHelp  = true,
    trigger   = 'tab',
}

local function update_mappings()
  local windows = get_app_windows()
  local mappings = {}
  for i, win in ipairs(windows) do
    if i<10 then
      table.insert(mappings,{ {}, string.char(i+string.byte('0')) , win:title()}) ;
    end
  end
  return mappings
end

local function update_actions()
  local windows = get_app_windows()
  local actions = {}
  for i,win in pairs(windows) do
    actions[win:title()] = function ()
      switch_to(win)
    end
  end
  return actions
end


return mode_comp.new_reactive("Switch Window Mode","F18",mode_config,update_mappings,update_actions)
