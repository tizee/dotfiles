local utils = require "config.utils"

local M = {}

function M:setup()

  -- nvim-treesitter
  require'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all" (the five listed parsers should always be installed)
    ensure_installed = { "c", "cpp", "vim", "rust", "javascript", "typescript", "tsx"},

    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = true,

    -- Automatically install missing parsers when entering buffer
    -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
    auto_install = false,

    -- List of parsers to ignore installing (or "all")
    ignore_install = { "" },

    -- The default paths are:
    --    first the package folder. Where nvim-treesitter is installed.
    --    second the site directory. This is the "site" subdirectory of stdpath("data").

    highlight = {
      enable = true,

      -- to disable slow treesitter highlight for large files
      disable = function(lang, buf)
          local max_filesize = 1024 * 1024 -- 1 MB
          local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
          if ok and stats and stats.size > max_filesize then
              return true
          end
      end,

      -- Using this option may slow down your editor, and you may see some duplicate highlights.
      additional_vim_regex_highlighting = false,
    },
  }

  -- swap-ternary.nvim
  vim.api.nvim_set_keymap('n', '<leader>s', ':lua require("swap-ternary").swap()<CR>', { noremap = true, silent = true })

end

return M
