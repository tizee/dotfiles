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
  -- Use faster method: get all windows for the app directly
  -- This avoids the slow window filter that queries current space
  local windows = app:allWindows()
  
  -- Filter for valid, visible windows only
  local validWindows = {}
  for _, win in ipairs(windows) do
    local title = win:title()
    -- Filter out windows that are:
    -- - not visible
    -- - minimized
    -- - have no title (nil or empty string)
    if win:isVisible() and not win:isMinimized() and 
       title and title ~= "" then
      table.insert(validWindows, win)
    end
  end
  
  return validWindows
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

-- Cache windows to avoid multiple calls to getAppWindows()
local cached_windows = nil

local function update_mappings()
  cached_windows = getAppWindows()
  local mappings = {}
  for i, win in ipairs(cached_windows) do
    if i < 10 then
      -- use number
      local title = string.sub(win:title() or "", 1, 50)
      table.insert(mappings, { {}, string.char(i + string.byte('0')), title })
    end
  end
  return mappings
end

local function update_actions()
  -- Use cached windows instead of calling getAppWindows() again
  local windows = cached_windows or {}
  local actions = {}
  for _, win in pairs(windows) do
    -- limit the length of title to 50 bytes
    local title = string.sub(win:title() or "", 1, 50)
    actions[title] = function()
      switchWindow(win)
    end
  end
  return actions
end


return mode_comp.new_reactive("Switch Window Mode","F18",mode_config,update_mappings,update_actions)
