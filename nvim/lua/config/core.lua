local utils = require 'config.utils'
local options = require 'config.options'
local lsp = require 'config.lsp'
local cmd = vim.cmd
vim.opt.packpath:append("~/.vim/plugged")

local load = function()
  -- vim compatible configurations
  cmd('source ' .. utils.nvim_config_path .. utils.path_sep  .. 'vimrc.vim')
  -- nvim only options
  options:load_options()
  -- nvim lsp
  lsp:setup()
end

load()
