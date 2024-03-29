-- luacheck: globals hs
-- a message widget
local drawing = require 'hs.drawing'
local screen = require 'hs.screen'
local styledtext = require 'hs.styledtext'

local statusmessage = {}
statusmessage.new = function(messageText)
  local buildParts = function(msg)
    local frame = screen.primaryScreen():frame()

    local styledTextAttributes = {
      font = { name = 'JetBrainsMono Nerd Font', size = 26 },
      color = { white = 1, alpha = 0.95}
    }

    local styledText = styledtext.new('MODE ' .. msg, styledTextAttributes)

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
    msg = messageText,
    show = function(self)
      self:hide()

      self.background, self.text = self._buildParts(self.msg)
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
      local secs = seconds or 1
      self:show()
      hs.timer.delayed.new(secs, function() self:hide() end):start()
    end
  }
end

return statusmessage
