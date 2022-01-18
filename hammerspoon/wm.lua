-- window manager utils
-- credit: https://github.com/jasonrudolph/keyboard
-- The wm is what I need now
local wm_utils = {}
function wm_utils.maximize(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f)
end
-- +-----------------+
-- |        |        |
-- |  HERE  |        |
-- |        |        |
-- +-----------------+
function wm_utils.left(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end

-- +-----------------+
-- |        |        |
-- |        |  HERE  |
-- |        |        |
-- +-----------------+
function wm_utils.right(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end

-- +-----------------+
-- |      HERE       |
-- +-----------------+
-- |                 |
-- +-----------------+
function wm_utils.up(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.w = max.w
  f.y = max.y
  f.h = max.h / 2
  win:setFrame(f)
end

-- +-----------------+
-- |                 |
-- +-----------------+
-- |      HERE       |
-- +-----------------+
function wm_utils.down(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.w = max.w
  f.y = max.y + (max.h / 2)
  f.h = max.h / 2
  win:setFrame(f)
end

-- +-----------------+
-- |  HERE  |        |
-- +--------+        |
-- |                 |
-- +-----------------+
function wm_utils.upLeft(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.x
  f.y = max.y
  f.w = max.w/2
  f.h = max.h/2
  win:setFrame(f)
end

-- +-----------------+
-- |                 |
-- +--------+        |
-- |  HERE  |        |
-- +-----------------+
function wm_utils.downLeft(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.x
  f.y = max.y + (max.h / 2)
  f.w = max.w/2
  f.h = max.h/2
  win:setFrame(f)
end

-- +-----------------+
-- |                 |
-- |        +--------|
-- |        |  HERE  |
-- +-----------------+
function wm_utils.downRight(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.x + (max.w / 2)
  f.y = max.y + (max.h / 2)
  f.w = max.w/2
  f.h = max.h/2

  win:setFrame(f)
end

-- +-----------------+
-- |        |  HERE  |
-- |        +--------|
-- |                 |
-- +-----------------+
function wm_utils.upRight(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w/2
  f.h = max.h/2
  win:setFrame(f)
end

-- +--------------+
-- |  |        |  |
-- |  |  HERE  |  |
-- |  |        |  |
-- +---------------+
function wm_utils.centerWithFullHeight(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.x + (max.w / 5)
  f.w = max.w * 3/5
  f.y = max.y
  f.h = max.h
  win:setFrame(f)
end

-- +-----------------+
-- |      |          |
-- | HERE |          |
-- |      |          |
-- +-----------------+
function wm_utils.left40(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w * 0.4
  f.h = max.h
  win:setFrame(f)
end

-- +-----------------+
-- |      |          |
-- |      |   HERE   |
-- |      |          |
-- +-----------------+
function wm_utils.right60(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w * 0.4)
  f.y = max.y
  f.w = max.w * 0.6
  f.h = max.h
  win:setFrame(f)
end

function wm_utils.nextScreen(win)
  local currentScreen = win:screen()
  local allScreens = hs.screen.allScreens()
  currentScreenIndex = hs.fnutils.indexOf(allScreens, currentScreen)
  nextScreenIndex = currentScreenIndex + 1

  if allScreens[nextScreenIndex] then
    win:moveToScreen(allScreens[nextScreenIndex])
  else
    win:moveToScreen(allScreens[1])
  end
end

local windowLayoutMode = hs.hotkey.modal.new({}, 'F16')

windowLayoutMode.entered = function()
  windowLayoutMode.statusMessage:show()
end
windowLayoutMode.exited = function()
  windowLayoutMode.statusMessage:hide()
end

-- Bind the given key to call the given function and exit WindowLayout mode
function windowLayoutMode.bindWithAutomaticExit(mode, modifiers, key, fn)
  mode:bind(modifiers, key, function()
    mode:exit()
    fn()
  end)
end

-- Share global config so we could configure options in just one place
local status, HS_Config = pcall(require, 'config')
if not status then
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
else
  windowMappings = HS_Config.windowMappings
end

local modifiers = windowMappings.modifiers
local showHelp  = windowMappings.showHelp
local trigger   = windowMappings.trigger
local mappings  = windowMappings.mappings

local function getModifiersStr(modifiers)
  local modMap = { shift = '⇧', ctrl = '⌃', alt = '⌥', cmd = '⌘' }
  local retVal = ''

  for i, v in ipairs(modifiers) do
    retVal = retVal .. modMap[v]
  end

  return retVal
end

local msgStr = getModifiersStr(modifiers)
msgStr = 'Window Layout Mode (' .. msgStr .. (string.len(msgStr) > 0 and '+' or '') .. trigger .. ')'

for i, mapping in ipairs(mappings) do
  local modifiers, trigger, winFunction = table.unpack(mapping)
  local hotKeyStr = getModifiersStr(modifiers)

  if showHelp == true then
    if string.len(hotKeyStr) > 0 then
      msgStr = msgStr .. (string.format('\n%10s+%s => %s', hotKeyStr, trigger, winFunction))
    else
      msgStr = msgStr .. (string.format('\n%11s => %s', trigger, winFunction))
    end
  end

  windowLayoutMode:bindWithAutomaticExit(modifiers, trigger, function()
    local fw = hs.window.focusedWindow()
    wm_utils[winFunction](fw)
  end)
end

-- message widget <-
local drawing = require 'hs.drawing'
local screen = require 'hs.screen'
local styledtext = require 'hs.styledtext'

local statusmessage = {}
statusmessage.new = function(messageText)
  local buildParts = function(messageText)
    local frame = screen.primaryScreen():frame()

    local styledTextAttributes = {
      font = { name = 'Fira Code Regular', size = 20 },
      color = { white = 1, alpha = 0.95}
    }

    local styledText = styledtext.new('🪟' .. messageText, styledTextAttributes)

    local styledTextSize = drawing.getTextDrawingSize(styledText)
    local textRect = {
      x = frame.w - styledTextSize.w - 40,
      y = frame.h - styledTextSize.h,
      w = styledTextSize.w + 40,
      h = styledTextSize.h + 40,
    }
    local text = drawing.text(textRect, styledText):setAlpha(0.9)

    local background = drawing.rectangle(
      {
        x = frame.w - styledTextSize.w - 45,
        y = frame.h - styledTextSize.h - 3,
        w = styledTextSize.w + 15,
        h = styledTextSize.h + 10
      }
    )
    background:setRoundedRectRadii(8, 8)
    background:setFillColor({ red = 0, green = 0, blue = 0, alpha=0.6 })

    return background, text
  end

  return {
    _buildParts = buildParts,
    show = function(self)
      self:hide()

      self.background, self.text = self._buildParts(messageText)
      self.background:show()
      self.text:show()
    end,
    hide = function(self)
      if self.background then
        self.background:delete()
        self.background = nil
      end
      if self.text then
        self.text:delete()
        self.text = nil
      end
    end,
    notify = function(self, seconds)
      local seconds = seconds or 1
      self:show()
      hs.timer.delayed.new(seconds, function() self:hide() end):start()
    end
  }
end
-- ->

windowLayoutMode.statusMessage = statusmessage.new(msgStr)

-- Use modifiers+trigger to toggle WindowLayout Mode
hs.hotkey.bind(modifiers, trigger, function()
  windowLayoutMode:enter()
end)
windowLayoutMode:bind(modifiers, trigger, function()
  windowLayoutMode:exit()
end)
