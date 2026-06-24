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
			float = { width = 0.4, height = 0.8, border = "rounded", title = " zhu " },
		},
		keymaps = { -- buffer-local session-window mappings
			prompt = "i",
			cancel = "c",
			sessions = "s",
			scroll_bottom = "G",
			next_message = "]m",
			prev_message = "[m",
			close = "q",
		},
		global_keymaps = { -- editor-wide; all disabled by default
			toggle = true, -- e.g. "<leader>zo" to toggle the window
			prompt = true, -- e.g. "<leader>za" to prompt
			sessions = false, -- e.g. "<leader>zs" to pick a session
			cancel = true, -- e.g. "<leader>zc" to cancel a turn
			model = false, -- e.g. "<leader>zm" to switch the model
			effort = false, -- e.g. "<leader>ze" to set the effort
			thinking = false, -- e.g. "<leader>zt" to set the thinking mode
			permission = false, -- e.g. "<leader>zp" to set the permission mode
			prompt_visual = true, -- e.g. "<leader>zv" to prompt with selection (visual mode)
			prompt_buffer = true, -- e.g. "<leader>zb" to prompt with whole buffer
			prompt_cursor = true, -- e.g. "<leader>zC" to prompt with cursor line
		},
	})
end

return M
