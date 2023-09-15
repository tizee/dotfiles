-- luacheck: globals mode hs
local mode_comp = require('mode')
local log = hs.logger.new('switch-window-mode','debug') -- luacheck: ignore

local function getAppWindows()
  -- find running app with given name
  local app = hs.window.focusedWindow():application()
  if not app then
    -- app = nil
    return {}
  end
  -- log.d(app:name())
  -- Spaces-aware windowfilters might experience a (sometimes significant) delay after every Space switch,
  -- since (due to OS X limitations) they must re-query for the list of all windows in the current Space every time.
  local wf = hs.window.filter.new(false):setAppFilter(app:name(),{visible=nil, currentSpace=nil})
  local windows = wf:getWindows()
  return windows
end

-- shortcuts to common Apps
local function switchWindow(window)
  -- log.d("switch to",window)
  if window then
    -- if minimized then unhide
    if window:isMinimized() then
      window:unminimize();
    end
    if window:isVisible() == false then
      window:raise();
    end
    window:focus();
  end
end

local mode_config = {
    modifiers = {'ctrl'},
    showHelp  = true,
    trigger   = 'tab',
}

local function update_mappings()
  local windows = getAppWindows()
  local mappings = {}
  for i, win in ipairs(windows) do
    if i<10 then
      -- use number
      table.insert(mappings,{ {}, string.char(i+string.byte('0')) , string.sub(win:title(),0,50)});
    end
  end
  return mappings
end

local function update_actions()
  local windows = getAppWindows()
  local actions = {}
  for _,win in pairs(windows) do
    -- limit the length of title to 50 bytes
    actions[string.sub(win:title(),0,50)] = function ()
      switchWindow(win)
    end
  end
  return actions
end


return mode_comp.new_reactive("Switch Window Mode","F18",mode_config,update_mappings,update_actions)
