-- Hand-rolled treesitter incremental selection.
--
-- The `main` branch of nvim-treesitter removed the built-in
-- `incremental_selection` module. This is a minimal reimplementation on top of
-- the Neovim core treesitter API that keeps our original keymaps:
--
--   <C-space>  init / grow selection to the next enclosing node
--   <bs>       shrink selection back to the previous node
--
-- We keep a per-buffer stack of selected nodes so that growing pushes and
-- shrinking pops.

local M = {}

-- Per-buffer node stack: { [bufnr] = { node1, node2, ... } } where the last
-- element is the currently visualised node.
local stacks = {}

local function get_stack(buf)
	local s = stacks[buf]
	if not s then
		s = {}
		stacks[buf] = s
		-- Drop the stack when the buffer is unloaded to avoid stale state.
		vim.api.nvim_buf_attach(buf, false, {
			on_detach = function()
				stacks[buf] = nil
			end,
		})
	end
	return s
end

-- Visually select the given node's range (charwise).
local function select_node(node)
	local srow, scol, erow, ecol = node:range()
	-- node:range() end column is exclusive; convert to an inclusive cursor pos.
	-- Move to start, enter charwise visual, move to last selected char.
	vim.api.nvim_win_set_cursor(0, { srow + 1, scol })
	vim.cmd("normal! v")
	local end_col = ecol > 0 and ecol - 1 or 0
	vim.api.nvim_win_set_cursor(0, { erow + 1, end_col })
end

-- Find the nearest ancestor whose range is strictly larger than `node`'s, so a
-- single keypress always produces a visible change. Returns nil at the root.
local function larger_ancestor(node)
	local sr, sc, er, ec = node:range()
	local parent = node:parent()
	while parent do
		local psr, psc, per, pec = parent:range()
		if psr ~= sr or psc ~= sc or per ~= er or pec ~= ec then
			return parent
		end
		parent = parent:parent()
	end
	return nil
end

function M.init_selection()
	local buf = vim.api.nvim_get_current_buf()
	local stack = get_stack(buf)
	-- Reset and seed with the node under the cursor.
	for i = #stack, 1, -1 do
		stack[i] = nil
	end
	local node = vim.treesitter.get_node()
	if not node then
		return
	end
	stack[#stack + 1] = node
	select_node(node)
end

function M.node_incremental()
	local buf = vim.api.nvim_get_current_buf()
	local stack = get_stack(buf)
	if #stack == 0 then
		-- No active selection yet: behave like init.
		M.init_selection()
		return
	end
	local current = stack[#stack]
	local bigger = larger_ancestor(current)
	if not bigger then
		-- Already at the outermost node; just re-select to keep visual mode.
		select_node(current)
		return
	end
	stack[#stack + 1] = bigger
	select_node(bigger)
end

function M.node_decremental()
	local buf = vim.api.nvim_get_current_buf()
	local stack = get_stack(buf)
	if #stack <= 1 then
		-- Nothing smaller to shrink to; keep the current selection.
		if #stack == 1 then
			select_node(stack[1])
		end
		return
	end
	stack[#stack] = nil
	select_node(stack[#stack])
end

function M.setup()
	-- Start / grow from normal mode and grow while already in visual mode.
	vim.keymap.set("n", "<C-space>", M.init_selection, { silent = true, desc = "TS init selection" })
	vim.keymap.set("x", "<C-space>", function()
		M.node_incremental()
	end, { silent = true, desc = "TS increment selection" })
	vim.keymap.set("x", "<bs>", function()
		M.node_decremental()
	end, { silent = true, desc = "TS decrement selection" })
end

return M
