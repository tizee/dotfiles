-- luacheck: globals hs
local alert = require("hs.alert")
local HS_Config = require('config')
local mode_comp = require('mode')
-- IME utils
local ime_utils= {}

function ime_utils.currentSourceID()
  alert.show(
  string.format(
   "%s",
   hs.keycodes.currentSourceID()
  )
 )

  local methods = hs.keycodes.methods(true) -- 获取所有输入法ID
  for _, v in pairs(methods) do
      print(v)
  end
end

function ime_utils.ChineseSimplified()
    -- Set input method to Simplified Chinese
    hs.keycodes.currentSourceID("com.apple.inputmethod.SCIM.Shuangpin")
    alert.show("Switched to Chinese Simplified (Shuangpin)")
end

function ime_utils.English()
    -- Set input method to English
    -- if hs.keycodes.currentSourceID() ~= "com.apple.keylayout.US" then
    --     hs.keycodes.currentSourceID("com.apple.keylayout.US")
    --     alert.show("Switched to English")
    -- end
    -- Set input method to English (Romaji)
    hs.keycodes.currentSourceID("com.apple.inputmethod.Kotoeri.RomanjiTyping.Roman")
    alert.show("Switched to English (Romaji)")
end

function ime_utils.ChineseTraditional()
    -- Set input method to Traditional Chinese
    hs.keycodes.currentSourceID("com.apple.inputmethod.TCIM.Shuangpin")
    alert.show("Switched to Chinese Traditional (Shuangpin)")
end

function ime_utils.JapaneseHiragana()
    -- Set input method to Japanese Hiragana
    hs.keycodes.currentSourceID("com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese")
    alert.show("Switched to Japanese Hiragana")
end

function ime_utils.JapaneseKatakana()
    -- Set input method to Japanese Katakana
    hs.keycodes.currentSourceID("com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese.Katakana")
    alert.show("Switched to Japanese Katakana")
end

-- IME mode
return mode_comp.new("IME Mode","F16",HS_Config.keyboardMappings,ime_utils)
