local M = {}

-- Parsers we want installed. On the `main` branch parser management moved out of
-- a giant `ensure_installed` table into an explicit install() call.
local ENSURE_INSTALLED = {
	"c",
	"cmake",
	"cpp",
	"dockerfile",
	"go",
	"javascript",
	"lua",
	"make",
	"markdown",
	"rust",
	"svelte",
	"tsx",
	"typescript",
	"vim",
	"vimdoc",
	"yaml",
}

-- Decide whether treesitter highlighting should be disabled for a buffer.
-- Mirrors the old `highlight.disable` callback: skip very large files to keep
-- the editor responsive. Returns true when highlighting should be SKIPPED.
local function should_disable(buf)
	-- Buffers with a huge number of lines: keep highlighting (matches old logic,
	-- which returned false -> do NOT disable -- for >50000 line buffers).
	if vim.api.nvim_buf_line_count(buf) > 50000 then
		return false
	end
	local max_filesize = 5 * 1024 * 1024 -- 5 MB
	local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
	if ok and stats and stats.size > max_filesize then
		return true
	end
	return false
end

function M.setup()
	-- 1. Core plugin setup + parser installation -----------------------------
	local nts = require("nvim-treesitter")
	nts.setup({
		-- Keep parsers/queries under stdpath('data')/site (default location).
		install_dir = vim.fn.stdpath("data") .. "/site",
	})

	-- Install parsers asynchronously. No-op if already present. This replaces
	-- the old `ensure_installed` + `sync_install` options.
	nts.install(ENSURE_INSTALLED)

	-- 2. Highlighting --------------------------------------------------------
	-- The `main` branch hands highlight/indent enabling back to the Neovim core.
	-- We enable them per-buffer via a FileType autocmd. Map our parser list to
	-- the filetypes that should trigger treesitter.
	local ts_filetypes = {
		"c",
		"cmake",
		"cpp",
		"dockerfile",
		"go",
		"javascript",
		"javascriptreact",
		"lua",
		"make",
		"markdown",
		"rust",
		"svelte",
		"typescript",
		"typescriptreact",
		"tsx",
		"vim",
		"help", -- vimdoc parser highlights `help` filetype
		"yaml",
	}

	local group = vim.api.nvim_create_augroup("tizee_treesitter", { clear = true })
	vim.api.nvim_create_autocmd("FileType", {
		group = group,
		pattern = ts_filetypes,
		callback = function(args)
			local buf = args.buf
			if should_disable(buf) then
				return
			end
			-- vim.treesitter.start() attaches the core highlighter. Guard with
			-- pcall so a missing parser never aborts buffer loading.
			pcall(vim.treesitter.start, buf)

			-- Experimental treesitter-based indentation (main branch).
			vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
		end,
	})

	-- 3. Incremental selection (hand-rolled) ---------------------------------
	-- The `main` branch removed the built-in `incremental_selection` module, so
	-- we reimplement the subset we used directly on top of the core API.
	require("tizee.plugins.ts-incremental-selection").setup()

	-- 4. Textobjects (select / swap / move) ----------------------------------
	require("nvim-treesitter-textobjects").setup({
		select = {
			lookahead = true,
			include_surrounding_whitespace = true,
		},
		move = {
			set_jumps = true,
		},
	})

	local select = require("nvim-treesitter-textobjects.select")
	local swap = require("nvim-treesitter-textobjects.swap")
	local move = require("nvim-treesitter-textobjects.move")

	-- select: { lhs, query, group, desc }
	local select_maps = {
		{ "a=", "@assignment.outer", "textobjects", "Select outer part of an assignment" },
		{ "i=", "@assignment.inner", "textobjects", "Select inner part of an assignment" },
		{ "l=", "@assignment.lhs", "textobjects", "Select left hand side of an assignment" },
		{ "r=", "@assignment.rhs", "textobjects", "Select right hand side of an assignment" },
		{ "aa", "@parameter.outer", "textobjects", "Select outer part of a parameter/argument" },
		{ "ia", "@parameter.inner", "textobjects", "Select inner part of a parameter/argument" },
		{ "ai", "@conditional.outer", "textobjects", "Select outer part of a conditional" },
		{ "ii", "@conditional.inner", "textobjects", "Select inner part of a conditional" },
		{ "al", "@loop.outer", "textobjects", "Select outer part of a loop" },
		{ "il", "@loop.inner", "textobjects", "Select inner part of a loop" },
		{ "af", "@call.outer", "textobjects", "Select outer part of a function call" },
		{ "if", "@call.inner", "textobjects", "Select inner part of a function call" },
		{ "am", "@function.outer", "textobjects", "Select outer part of a method/function definition" },
		{ "im", "@function.inner", "textobjects", "Select inner part of a method/function definition" },
		{ "ac", "@class.outer", "textobjects", "Select outer part of a class" },
		{ "ic", "@class.inner", "textobjects", "Select inner part of a class" },
		{ "as", "@scope", "locals", "Select language scope" },
	}
	for _, m in ipairs(select_maps) do
		local lhs, query, query_group, desc = m[1], m[2], m[3], m[4]
		vim.keymap.set({ "x", "o" }, lhs, function()
			select.select_textobject(query, query_group)
		end, { desc = desc })
	end

	-- swap
	vim.keymap.set("n", "<leader>na", function()
		swap.swap_next("@parameter.inner")
	end, { desc = "Swap next parameter" })
	vim.keymap.set("n", "<leader>nf", function()
		swap.swap_next("@function.outer")
	end, { desc = "Swap next function" })
	vim.keymap.set("n", "<leader>pa", function()
		swap.swap_previous("@parameter.inner")
	end, { desc = "Swap previous parameter" })
	vim.keymap.set("n", "<leader>pf", function()
		swap.swap_previous("@function.outer")
	end, { desc = "Swap previous function" })

	-- move: { lhs, fn, query, group, desc }
	local move_maps = {
		-- goto_next_start
		{ "]f", move.goto_next_start, "@call.outer", "textobjects", "Next function call start" },
		{ "]m", move.goto_next_start, "@function.outer", "textobjects", "Next method/function def start" },
		{ "]c", move.goto_next_start, "@class.outer", "textobjects", "Next class start" },
		{ "]i", move.goto_next_start, "@conditional.outer", "textobjects", "Next conditional start" },
		{ "]l", move.goto_next_start, "@loop.outer", "textobjects", "Next loop start" },
		{ "]s", move.goto_next_start, "@scope", "locals", "Next scope" },
		{ "]z", move.goto_next_start, "@fold", "folds", "Next fold" },
		-- goto_next_end
		{ "]F", move.goto_next_end, "@call.outer", "textobjects", "Next function call end" },
		{ "]M", move.goto_next_end, "@function.outer", "textobjects", "Next method/function def end" },
		{ "]C", move.goto_next_end, "@class.outer", "textobjects", "Next class end" },
		{ "]I", move.goto_next_end, "@conditional.outer", "textobjects", "Next conditional end" },
		{ "]L", move.goto_next_end, "@loop.outer", "textobjects", "Next loop end" },
		-- goto_previous_start
		{ "[f", move.goto_previous_start, "@call.outer", "textobjects", "Prev function call start" },
		{ "[m", move.goto_previous_start, "@function.outer", "textobjects", "Prev method/function def start" },
		{ "[c", move.goto_previous_start, "@class.outer", "textobjects", "Prev class start" },
		{ "[i", move.goto_previous_start, "@conditional.outer", "textobjects", "Prev conditional start" },
		{ "[l", move.goto_previous_start, "@loop.outer", "textobjects", "Prev loop start" },
		-- goto_previous_end
		{ "[F", move.goto_previous_end, "@call.outer", "textobjects", "Prev function call end" },
		{ "[M", move.goto_previous_end, "@function.outer", "textobjects", "Prev method/function def end" },
		{ "[C", move.goto_previous_end, "@class.outer", "textobjects", "Prev class end" },
		{ "[I", move.goto_previous_end, "@conditional.outer", "textobjects", "Prev conditional end" },
		{ "[L", move.goto_previous_end, "@loop.outer", "textobjects", "Prev loop end" },
	}
	for _, m in ipairs(move_maps) do
		local lhs, fn, query, query_group, desc = m[1], m[2], m[3], m[4], m[5]
		vim.keymap.set({ "n", "x", "o" }, lhs, function()
			fn(query, query_group)
		end, { desc = desc })
	end

	-- 5. swap-ternary.nvim (unrelated plugin, preserved) ---------------------
	vim.api.nvim_set_keymap(
		"n",
		"<leader>S",
		':lua require("swap-ternary").swap()<CR>',
		{ noremap = true, silent = true }
	)

	-- 6. Repeatable moves ----------------------------------------------------
	-- Module path changed on main branch: nvim-treesitter-textobjects.repeatable_move
	-- builtin_*_expr replaces builtin_* and must be set with { expr = true }.
	local ts_repeat_move = require("nvim-treesitter-textobjects.repeatable_move")

	-- vim way: ; goes to the direction you were moving.
	vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
	vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)
	-- Make builtin f, F, t, T also repeatable with ; and ,
	vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f_expr, { expr = true })
	vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F_expr, { expr = true })
	vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t_expr, { expr = true })
	vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T_expr, { expr = true })
end

return M
