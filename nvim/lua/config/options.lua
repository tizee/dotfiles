-- Vim Options
local opt = vim.opt -- table of options
local options = setmetatable({}, {__index = { global_local = {} } })

-- nvim only options
function options:load_options()
  self.global_local = {
    -- shows the effects of a command incrementally as you type
    -- inccommand = "nosplit";
    secure = false;
  }
  for name,val in pairs(self.global_local) do
    opt[name] = val
  end
end

return options
