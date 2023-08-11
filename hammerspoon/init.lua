-- hotkey debug
require('hs.hotkey').setLogLevel("warning")

-- local var from hammerspoon
local hotkey = require('hs.hotkey')
local alert = require('hs.alert')
local application = require('hs.application')
local pathwatcher = require('hs.pathwatcher')
local hints = require('hs.hints')
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
hints.fontName = "JetBrainsMono Nerd Font"
hints.fontSize = 24
hints.iconAlpha = 1.0
hints.showTitleThresh = 0

-- hs alert style
alert.defaultStyle.strokeColor = {white = 1, alpha = 0}
alert.defaultStyle.fillColor = {white = 0.05, alpha = 0.85}
alert.defaultStyle.radius = 8
alert.defaultStyle.fadeOutDuration = 0.8
alert.defaultStyle.textFont = "JetBrainsMono Nerd Font"
alert.defaultStyle.textSize = 30
-- ->

-- keybindings <-

require("window-layout-mode")
require("quick-action-mode")
require("switch-window-mode")


-- menubar item
local caffeine = hs.menubar.new()
local awake_icon= hs.image.imageFromPath("~/.config/hammerspoon/icons/sun.max@2x.png")
local sleepy_icon = hs.image.imageFromPath("~/.config/hammerspoon/icons/bed.double.fill@2x.png")

local function setCaffeineDisplay(state)
  print(caffeine:icon())
    if state then
        caffeine:setIcon(awake_icon,false)
print("awake", awake_icon)
    else
        caffeine:setIcon(sleepy_icon,false)
print("sleepy", sleepy_icon)
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

-- auto-reload config
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

alert.show(string.format("Config loaded under\n %s",CONFIG_PATH))
-- vim:foldmarker=<-,-> foldmethod=marker
