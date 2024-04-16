local M = {}

function M.setup()
	-- nvim-treesitter
	require("nvim-treesitter.configs").setup({
		-- parser dir to package dir
		-- The default paths are:
		--    first the package folder. Where nvim-treesitter is installed.
		--    second the site directory. This is the "site" subdirectory of stdpath("data").
		-- parser_install_dir = nil,
		-- A list of parser names, or "all" (the five listed parsers should always be installed)
		ensure_installed = {
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
		},

		-- Install parsers synchronously (only applied to `ensure_installed`)
		sync_install = true,

		-- Automatically install missing parsers when entering buffer
		-- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
		auto_install = false,

		-- List of parsers to ignore installing (or "all")
		ignore_install = {},

		-- indentation
		indent = { enable = true },

		highlight = {
			enable = true,

			-- to disable slow treesitter highlight for large files
			disable = function(lang, buf)
				if vim.api.nvim_buf_line_count(buf) > 50000 then
					return false
				end
				local max_filesize = 5 * 1024 * 1024 -- 5 MB
				local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
				if ok and stats and stats.size > max_filesize then
					return true
				end
				return false
			end,

			-- Using this option may slow down your editor, and you may see some duplicate highlights.
			additional_vim_regex_highlighting = false,
		},

		incremental_selection = {
			enable = true,
			keymaps = {
				-- {function:keymap}
				-- https://github.com/nvim-treesitter/nvim-treesitter/blob/ef267f0c285928ea3a0d3362a260a0728fd4a146/lua/nvim-treesitter/incremental_selection.lua#L154
				init_selection = "<C-space>",
				node_incremental = "<C-space>",
				scope_incremental = false,
				node_decremental = "<bs>",
			},
		},

		-- nvim-treesitter-textobjects
		textobjects = {
			-- text object for selection
			select = {
				enable = true,

				-- jump forward to textobj
				lookahead = true,

				keymaps = {
					["a="] = { query = "@assignment.outer", desc = "Select outer part of an assignment" },
					["i="] = { query = "@assignment.inner", desc = "Select inner part of an assignment" },
					["l="] = { query = "@assignment.lhs", desc = "Select left hand side of an assignment" },
					["r="] = { query = "@assignment.rhs", desc = "Select right hand side of an assignment" },

					["aa"] = { query = "@parameter.outer", desc = "Select outer part of a parameter/argument" },
					["ia"] = { query = "@parameter.inner", desc = "Select inner part of a parameter/argument" },

					["ai"] = { query = "@conditional.outer", desc = "Select outer part of a conditional" },
					["ii"] = { query = "@conditional.inner", desc = "Select inner part of a conditional" },

					["al"] = { query = "@loop.outer", desc = "Select outer part of a loop" },
					["il"] = { query = "@loop.inner", desc = "Select inner part of a loop" },

					["af"] = { query = "@call.outer", desc = "Select outer part of a function call" },
					["if"] = { query = "@call.inner", desc = "Select inner part of a function call" },

					["am"] = { query = "@function.outer", desc = "Select outer part of a method/function definition" },
					["im"] = { query = "@function.inner", desc = "Select inner part of a method/function definition" },

					["ac"] = { query = "@class.outer", desc = "Select outer part of a class" },
					["ic"] = { query = "@class.inner", desc = "Select inner part of a class" },
					-- You can also use captures from other query groups like `locals.scm`
					["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
				},

				include_surrounding_whitespace = true,
			},

			-- swap text objects
			swap = {
				enable = true,
				swap_next = {
					["<leader>na"] = "@parameter.inner",
					["<leader>nf"] = "@function.outer",
				},
				swap_previous = {
					["<leader>pa"] = "@parameter.inner",
					["<leader>pf"] = "@function.outer",
				},
			},

			-- navigate using textojbects
			move = {
				enable = true,
				-- set jumps in the jumplist
				set_jumps = true,
				goto_next_start = {
					["]f"] = { query = "@call.outer", desc = "Next function call start" },
					["]m"] = { query = "@function.outer", desc = "Next method/function def start" },
					["]c"] = { query = "@class.outer", desc = "Next class start" },
					["]i"] = { query = "@conditional.outer", desc = "Next conditional start" },
					["]l"] = { query = "@loop.outer", desc = "Next loop start" },

					-- You can pass a query group to use query from `queries/<lang>/<query_group>.scm file in your runtime path.
					-- Below example nvim-treesitter's `locals.scm` and `folds.scm`. They also provide highlights.scm and indent.scm.
					["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
					["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
				},

				goto_next_end = {
					["]F"] = { query = "@call.outer", desc = "Next function call end" },
					["]M"] = { query = "@function.outer", desc = "Next method/function def end" },
					["]C"] = { query = "@class.outer", desc = "Next class end" },
					["]I"] = { query = "@conditional.outer", desc = "Next conditional end" },
					["]L"] = { query = "@loop.outer", desc = "Next loop end" },
				},

				goto_previous_start = {
					["[f"] = { query = "@call.outer", desc = "Prev function call start" },
					["[m"] = { query = "@function.outer", desc = "Prev method/function def start" },
					["[c"] = { query = "@class.outer", desc = "Prev class start" },
					["[i"] = { query = "@conditional.outer", desc = "Prev conditional start" },
					["[l"] = { query = "@loop.outer", desc = "Prev loop start" },
				},

				goto_previous_end = {
					["[F"] = { query = "@call.outer", desc = "Prev function call end" },
					["[M"] = { query = "@function.outer", desc = "Prev method/function def end" },
					["[C"] = { query = "@class.outer", desc = "Prev class end" },
					["[I"] = { query = "@conditional.outer", desc = "Prev conditional end" },
					["[L"] = { query = "@loop.outer", desc = "Prev loop end" },
				},
			},
		},

		-- nvim-treesitter-refactor
		refactor = {
			-- Highlights definition and usages of the current symbol under the cursor.
			highlight_definitions = {
				enable = true,
				-- Set to false if you have an `updatetime` of ~100.
				clear_on_cursor_move = false,
			},

			-- Highlights the block from the current scope where the cursor is.
			highlight_current_scope = { enable = true },

			-- Renames the symbol under the cursor within the current scope (and current file).
			smart_rename = {
				enable = true,
				-- Assign keymaps to false to disable them, e.g. `smart_rename = false`.
				keymaps = {
					smart_rename = "grr",
				},
			},

			-- Provides "go to definition" for the symbol under the cursor, and lists the definitions from the current file.
			navigation = {
				enable = false,
				-- Assign keymaps to false to disable them, e.g. `goto_definition = false`.
				keymaps = false,
			},
		},
	})

	-- swap-ternary.nvim
	vim.api.nvim_set_keymap(
		"n",
		"<leader>S",
		':lua require("swap-ternary").swap()<CR>',
		{ noremap = true, silent = true }
	)

	local ts_repeat_move = require("nvim-treesitter.textobjects.repeatable_move")

	-- vim way: ; goes to the direction you were moving.
	vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
	vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)
	-- Optionally, make builtin f, F, t, T also repeatable with ; and ,
	vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f)
	vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F)
	vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t)
	vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T)
end

return M
