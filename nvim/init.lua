-- nvim-treesitter (main branch) installs parsers and queries under
-- stdpath('data')/site and prepends that dir to runtimepath itself (see its
-- setup({ install_dir = ... })). vim-plug already adds the plugin to
-- runtimepath at plug#end(). Manually prepending ~/.vim/plugged/nvim-treesitter
-- here is not only redundant, it lets stale parser/*.so files left over from
-- the old `master` branch shadow the freshly built parsers in site/, causing
-- parser/query version mismatches (E5113 "Invalid field name"). So we no longer
-- touch runtimepath for treesitter and only sync packpath to runtimepath.
vim.api.nvim_command('let &packpath = &runtimepath')
require("tizee.core").setup()
