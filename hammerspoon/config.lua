-- keymappings
return {
	-- whether auto-reload configuration when changed
	auto_reload = false,
	keyboardMappings = {
		modifiers = { "ctrl" },
		showHelp = true,
		trigger = "s",
		mappings = {
			{ {}, "i", "currentSourceID", "Show Current Input Source"},
			{ {}, "h", "ChineseTraditional", "Traditional Chinese"},
			{ {}, "j", "English", "English"},
			{ {}, "k", "ChineseSimplified", "Simplified Chinese"},
			{ {}, "l", "JapaneseHiragana", "Japanese Hiragana"},
			{ {}, "o", "JapaneseKatakana", "Japanese Katakana"},
		}
	},
	-- windowMappings = {
	-- 	modifiers = { "ctrl" },
	-- 	showHelp = true,
	-- 	trigger = "s",
	-- 	mappings = {
	-- 		{ {}, "return", "maximize" },
	-- 		{ {}, "space", "centerWithFullHeight" },
	-- 		{ {}, "h", "left" },
	-- 		{ {}, "j", "down" },
	-- 		{ {}, "k", "up" },
	-- 		{ {}, "l", "right" },
	-- 		{ { "shift" }, "h", "left40" },
	-- 		{ { "shift" }, "l", "right60" },
	-- 		{ {}, "i", "upLeft" },
	-- 		{ {}, "o", "upRight" },
	-- 		{ {}, ",", "downLeft" },
	-- 		{ {}, ".", "downRight" },
	-- 		{ {}, "1", "win1140x700" },
			-- { {},         kn',      'nextScreen' },
			-- { {},         'right',  'moveOneScreenEast' },
			-- { {},         'left',   'moveOneScreenWest' },
		-- },
	-- },
	quickActionMappings = {
		modifiers = { "ctrl" },
		showHelp = true,
		trigger = "\\",
		mappings = {
			{ {}, "q", "quickNavigation", "Quick Navigation" },
			{ {}, "p", "pasteInPlainText", "Paste as Plain Text" },
			{ {}, "a", "showAppInfo", "Show App Info" },
			{ {}, "d", "toggleDarkMode", "Toggle Dark Mode" },
			{ {}, "r", "reloadConfig", "Reload Config" },
			{ {}, "y", "pasteBoardYoutubeURL", "YouTube URL Cleaner" },
			{ {}, "t", "pasteBoardRemoveTrackers", "Remove URL Trackers" },
			{ {}, "h", "Claude", "Toggle Claude" },
			{ {}, "j", "Ghostty", "Toggle Ghostty" },
			{ {}, "k", "Obsidian", "Toggle Obsidian" },
			{ {}, "i", "Cherry Studio", "Toggle Cherry Studio" },
		},
	},
}
