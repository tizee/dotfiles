-- Load Lua configuartion
require("config.core")
-- Load GUI configuration for NeoVide

-- neovim globals
if vim.fn.has("gui_running") and vim.fn.has("nvim") then
    vim.opt.guifont = { "JetBrainsMono Nerd Font:h16" }
		-- CJK characters
		-- LXGW WenKai Mono for Chinese characters
		-- Klee Medium for Japanese Kanji
    vim.opt.guifontwide = { "Hiragino Mincho Pro:h16" }
end
