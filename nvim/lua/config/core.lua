local global = require 'config.utils'
local options = require 'config.options'
local cmd = vim.cmd

local load = function()
  -- vim compatible configurations
  cmd('source ' .. global.nvim_config_path .. global.path_sep  .. 'vimrc.vim')
  -- nvim only options
  options:load_options()
  -- TODO nvim lsp
end

load()
