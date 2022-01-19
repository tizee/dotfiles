HS_Config = require('config')
local mode_comp = require('mode')
local application = require('hs.application')
local osascript = require('hs.osascript')
local CONFIG_PATH = os.getenv("HOME") .. "/.hammerspoon/"

local function toggle_app(name)
  -- find running app with given name
  local app = application.find(name)
  if not app then
    -- app = nil
    application.launchOrFocus(name)
    return
  end
  -- toggle max window
  local mainWin = app:mainWindow()
  if not mainWin then
    -- no windows
    application.launchOrFocus(name)
  elseif app:isFrontmost() == true then
    mainWin:application():hide()
  else
    -- bring front most all its windows
    mainWin:application():activate(true)
    mainWin:application():unhide()
    mainWin:focus()
  end
end

-- shortcuts to common Apps
local actions= {}
-- action name to app name
local app_names= {
 {"Firefox","Firefox Developer Edition"},
 {"OmniFocus","OmniFocus"},
 {"NeteaseMusic","NeteaseMusic"},
 {"Obsidian","Obsidian"},
 {"Telegram","Telegram"},
 {"Finder","Finder"}}

for key,row in ipairs(app_names) do
  local action, name = table.unpack(row)
  actions[action] = function ()
    toggle_app(name)
  end
end

function actions.reloadConfig()
  hs.reload()
end

-- read file content
local function read_file_content(full_path)
  -- make sure resolve symlink
  local file = io.open(full_path,"r")
  local data = file:read("*a")
  file:close()
  return data
end

-- toggle dark mode
function actions.toggleDarkMode ()
  osascript.applescript(read_file_content(CONFIG_PATH .. "scripts/" .. "toggle_darkmode.applescript"))
  hs.alert.show("Toggle Dark Mode")
end

return mode_comp.new("Quick Action Mode","F17",HS_Config.quickActionMappings,actions)
