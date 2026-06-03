-- Custom health checks for this config, surfaced via `:checkhealth tizee`.
--
-- Neovim discovers this module because it lives at `lua/tizee/health.lua` on
-- the runtimepath and exposes a `check()` function using the `vim.health`
-- reporter API.

local M = {}

local function check_treesitter()
	vim.health.start("tizee: nvim-treesitter (main branch)")

	local ts = require("tizee.plugins.nvim-treesitter")
	local ok, msg = ts.check()
	if ok then
		vim.health.ok(msg)
	else
		vim.health.error(msg, {
			"Install the CLI: brew install tree-sitter-cli",
			"Ensure Neovim's PATH includes the install dir (e.g. /opt/homebrew/bin).",
			"GUI-launched Neovim may not inherit your shell PATH.",
		})
	end
end

function M.check()
	check_treesitter()
end

return M
