local utils = require("config.utils")
local options = require("config.options")
local cmd = vim.cmd

local load = function()
	-- vim compatible configurations
	cmd("source " .. utils.nvim_config_path .. utils.path_sep .. "vimrc.vim")
	-- nvim only options
	options:load_options()
	-- plugins
	-- nvim treesitter
	local treesitter = require("config.plugins.nvim-treesitter")
	treesitter:setup()
end

load()
