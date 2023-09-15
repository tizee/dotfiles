-- luacheck: globals hs
-- Press Cmd+Q twice to quit
-- credit: https://github.com/pseudoyu/dotfiles/blob/master/hammerspoon/double_cmdq_to_quit.lua

local confirmModal = hs.hotkey.modal.new('cmd','q')

function confirmModal:entered()
    hs.alert.show("Press Cmd+Q again to confirm (Esc/q to close modal)", 1)
    hs.timer.doAfter(1, function() confirmModal:exit() end)
end

local function confirmQuit()
    local app = hs.window.focusedWindow():application()
    app:kill()
end

confirmModal:bind('cmd', 'q', confirmQuit)
confirmModal:bind('', 'escape', function() confirmModal:exit() end)
confirmModal:bind('', 'q', function() confirmModal:exit() end)
