-- hotkey debug
require('hs.hotkey').setLogLevel("warning")

-- local var from hammerspoon
local hotkey = require('hs.hotkey')
local alert = require('hs.alert')
local application = require('hs.application')
local pathwatcher = require('hs.pathwatcher')
local hints = require('hs.hints')
local osascript = require('hs.osascript')
local grid = require('hs.grid')

-- hyper key
-- idea from https://github.com/jasonrudolph/keyboard
local hyper = {"cmd","shift"}
local CONFIG_PATH = os.getenv("HOME") .. "/.hammerspoon/"

-- config <-
HS_Config = require('config')

-- no animation
hs.window.animationDuration = 0.0

-- This controls the set of characters that will be used for window hints. They must be characters found in hs.keycodes.map
hints.hintChars = {"J","K","H","L","A","S","D","F"}
hints.fontName = "Fira Code Regular"
hints.fontSize = 24
hints.iconAlpha = 1.0
hints.showTitleThresh = 0

-- quick navigation
hotkey.bind("ctrl","tab",function ()
  hints.windowHints()
end)


-- hs alert style
alert.defaultStyle.strokeColor = {white = 1, alpha = 0}
alert.defaultStyle.fillColor = {white = 0.05, alpha = 0.85}
alert.defaultStyle.radius = 8
alert.defaultStyle.fadeOutDuration = 0.8
alert.defaultStyle.textFont = "Fira Code Regular"
alert.defaultStyle.textSize = 30
-- ->

-- util <-
-- read file content
local function read_file_content(full_path)
  -- make sure resolve symlink
  local file = io.open(full_path,"r")
  local data = file:read("*a")
  file:close()
  return data
end

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

-- ->

-- ehanced macOS <-

-- window manager
require("wm")

-- Move/resize windows within a grid <-
grid.GRIDWIDTH = 8
grid.GRIDHEIGHT = 8
grid.MARGINX = 0
grid.MARGINY = 0

local gw = grid.GRIDWIDTH
local gh = grid.GRIDHEIGHT

local grid_config = {
  middle = {x = 1, y = 1, w = 6, h = 6},
  center = {x = 1, y = 1, w = 4, h = 4},
  large = {x = 0, y = 0, w = gw, h = gh},
}

hotkey.bind(
    hyper,
    "G",
    function()
        grid.set(hs.window.focusedWindow(), grid_config.middle)
    end
)
hotkey.bind(
    hyper,
    "H",
    function()
        grid.set(hs.window.focusedWindow(), grid_config.center)
    end
)
hotkey.bind(
    hyper,
    "L",
    function()
        grid.set(hs.window.focusedWindow(), grid_config.large)
    end
)
-- ->

-- Show current App Info
hotkey.bind(
    hyper,
    "i",
    function()
        alert.show(
            string.format(
                "App path:      %s\nApp name:      %s\nIM source id:  %s",
                hs.window.focusedWindow():application():path(),
                hs.window.focusedWindow():application():name(),
                hs.keycodes.currentSourceID()
            )
        )
    end
)

-- shortcuts to common Apps
local apps = {
  -- browser
  b = "Firefox Developer Edition",
  -- It's duplicate with my skhd settings but I don't mind
  t = "WezTerm",
  j = "Telegram",
  -- music
  m = "NeteaseMusic",
  f = "Finder",
}
for key,app in pairs(apps) do
  hotkey.bind(hyper,key,function ()
    toggle_app(app)
  end)
end

-- toggle dark mode
hotkey.bind(hyper,"D",function ()
  osascript.applescript(read_file_content(CONFIG_PATH .. "scripts/" .. "toggle_darkmode.applescript"))
  alert.show("Toggle Dark Mode")
end)

-- simple menubar item
local caffeine = hs.menubar.new()
local function setCaffeineDisplay(state)
    if state then
        caffeine:setTitle("AWAKE")    else
        caffeine:setTitle("SLEEPY")
    end
end

local function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

-- Bring Finder.app windows to front when focus
local function applicationWatcher(appName, eventType, appObject)
    if (eventType == application.watcher.activated) then
        if (appName == "Finder") then
            -- Bring all Finder windows forward when one gets activated
            appObject:selectMenuItem({"Window", "Bring All to Front"})
        end
    end
end
local finderWatcher = application.watcher.new(applicationWatcher)
finderWatcher:start()
-- ->

-- reload config
local function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
if HS_Config.auto_reload then
  local configWatcher = pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig)
  configWatcher.start()
end

hotkey.bind(hyper, "R", function()
  hs.reload()
end)
alert.show(string.format("Config loaded under\n %s",CONFIG_PATH))
-- vim:foldmarker=<-,-> foldmethod=marker
