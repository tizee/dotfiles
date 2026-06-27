local M = {}
function M.setup()
	local options = require("tizee.core.options")
	local config_path = vim.fn.stdpath("config")
	-- plugins
	-- Load GUI configuration for NeoVide
	-- neovim globals
	if vim.fn.has("gui_running") and vim.fn.has("nvim") then
		vim.opt.guifont = { "JetBrainsMono Nerd Font Mono:h16" }
		-- CJK characters
		-- LXGW WenKai Mono for Chinese characters
		-- Klee Medium for Japanese Kanji
		vim.opt.guifontwide = { "Hiragino Mincho Pro:h16" }
	end
	-- vim compatible configurations
	local cmd = vim.cmd
	-- load options
	-- load plugins managed by vim-plug
	cmd("source " .. config_path .. "/vimrc.vim")
	-- nvim treesitter
	local treesitter = require("tizee.plugins.nvim-treesitter")
	treesitter.setup()
	-- llms agent
	vim.opt.runtimepath:append("/Users/tizee/projects/project-AI/agents/zhu/neovim")
	vim.cmd("runtime plugin/zhu.lua")
	require("zhu").setup({
		command = { "zhu", "rpc" }, -- argv used to spawn the agent server
		review_edits = false, -- gate buffer edits behind a diff confirmation
		ephemeral = true, -- sessions are not persisted to disk
		window = { -- session window placement and appearance
			position = "float", -- right | left | bottom | top | float
			width = 64, -- columns for a vertical split
			height = 15, -- rows for a horizontal split
			wrap = true,
			float = { width = 0.35, height = 0.5, row = 0, col = 1.0, border = "rounded", title = " zhu " },
		},
		keymaps = { -- buffer-local session-window mappings
			prompt = "i",
			cancel = "c",
			clear = "C",
			sessions = "s",
			scroll_bottom = "G",
			next_message = "]m",
			prev_message = "[m",
			close = "q",
		},
		global_keymaps = { -- editor-wide; all disabled by default
			toggle = true, -- <leader>zo to toggle the window
			prompt = true, -- <leader>za to prompt
			sessions = true, -- <leader>zs to pick a session
			cancel = true, -- <leader>zc to cancel a turn
			model = true, -- <leader>zm to switch the model
			effort = true, -- <leader>ze to set the effort
			thinking = true, -- <leader>zt to set the thinking mode
			permission = true, -- <leader>zp to set the permission mode
			clear = true, -- <leader>zL to reset session and clear transcript
			prompt_visual = true, -- <leader>zv to prompt with selection (visual mode)
			prompt_buffer = true, -- <leader>zb to prompt with whole buffer
			prompt_cursor = true, -- <leader>zC to prompt with cursor line
		},
	})
end

return M
