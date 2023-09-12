-- Load Lua configuartion
require("config.core")
-- Load GUI configuration for NeoVide

-- neovim globals
if vim.fn.has("gui_running") and vim.fn.has("nvim") then
    vim.opt.guifont = { "JetBrainsMono Nerd Font:h16" }
    vim.opt.guifontwide = { "LXGW WenKai Mono:h16" }
end
