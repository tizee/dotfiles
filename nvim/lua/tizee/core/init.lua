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
end

return M
