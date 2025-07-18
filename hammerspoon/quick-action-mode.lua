-- luacheck: globals hs mode config
local HS_Config = require("config")
local mode_comp = require("mode")
local alert = require("hs.alert")
local hints = require("hs.hints")
local application = require("hs.application")
local osascript = require("hs.osascript")
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
local actions = {}

-- readable action names mapping
local action_display_names = {
	quickNavigation = "Quick Navigation",
	pasteInPlainText = "Paste as Plain Text",
	showAppInfo = "Show App Info",
	toggleDarkMode = "Toggle Dark Mode",
	reloadConfig = "Reload Config",
	pasteBoardYoutubeURL = "YouTube URL Cleaner",
	pasteBoardRemoveTrackers = "Remove URL Trackers",
	Claude = "Toggle Claude",
	Ghostty = "Toggle Ghostty",
	Obsidian = "Toggle Obsidian",
	["Cherry Studio"] = "Toggle Cherry Studio"
}

-- action name to app name
local app_names = {
	{ "Claude", "Claude" },
	{ "Ghostty", "Ghostty" },
	{ "Obsidian", "Obsidian" },
	{ "Cherry Studio", "Cherry Studio" }
}

for _, row in ipairs(app_names) do
	local action, name = table.unpack(row)
	actions[action] = function()
		toggle_app(name)
	end
end

-- quick navigation
function actions.quickNavigation()
	hints.windowHints()
end

-- show app info
function actions.showAppInfo()
	alert.show(
		string.format(
			"App path:      %s\nApp Name:      %s\nIM source id:  %s\nWindow Size: width:%s height:%s",
			hs.window.focusedWindow():application():path(),
			hs.window.focusedWindow():application():name(),
			hs.keycodes.currentSourceID(),
			hs.window.focusedWindow():size().w,
			hs.window.focusedWindow():size().h
		)
	)
end

function actions.reloadConfig()
	hs.reload()
end

-- read file content
local function read_file_content(full_path)
	-- make sure resolve symlink
	local file = io.open(full_path, "r")
	local data = file:read("*a")
	file:close()
	return data
end

function actions.pasteInPlainText()
	local rewriter = require("paste-in-plain-text")
	rewriter.pasteInPlainText()
end

function actions.pasteBoardRemoveTrackers()
	local rewriter = require("pasteboard-link-rewriter")
	rewriter.remove_link_tracker()
end

function actions.pasteBoardYoutubeURL()
	local rewriter = require("pasteboard-link-rewriter")
	rewriter.rewrite_youtube_link()
end

-- toggle dark mode
function actions.toggleDarkMode()
	osascript.applescript(read_file_content(CONFIG_PATH .. "scripts/" .. "toggle_darkmode.applescript"))
	hs.alert.show("Toggle Dark Mode")
end

-- function to get display name for an action
function actions.getDisplayName(action_key)
	return action_display_names[action_key] or action_key
end

return mode_comp.new("Quick Action Mode", "F17", HS_Config.quickActionMappings, actions)
