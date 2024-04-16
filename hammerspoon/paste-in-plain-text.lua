local alert = require("hs.alert")
local pb = require("hs.pasteboard")
local M = {}

function M.pasteInPlainText()
	local pasteboardText = pb.readString()
	print(pasteboardText)
	if pasteboardText then
		pb.setContents(pasteboardText)
		alert.show(string.format("content: %s", pasteboardText))
	else
		alert.show("Empty pasteboard")
	end
end

return M
