-- fix treesitter parser missing issue
vim.api.nvim_command('set runtimepath^=~/.vim/plugged/nvim-treesitter')
vim.api.nvim_command('let &packpath = &runtimepath')
require("tizee.core").setup()
