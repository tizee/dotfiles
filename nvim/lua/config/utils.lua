-- global variables
local global = {}
local home = os.getenv('HOME')
local path_sep_comp = global.is_windows and '\\' or '/' -- win:\\ unix:/
local os_name = vim.loop.os_uname().sysname -- $SYSTEM

function global:load_variables()
	self.is_mac = os_name == 'Darwin'
	self.is_linux = os_name == 'Linux'
	self.is_windows = os_name == 'Windows'
  self.path_sep = path_sep_comp
	self.home = home
  self.nvim_config_path = home .. path_sep_comp .. '.config' .. path_sep_comp .. 'nvim' .. path_sep_comp
end

global:load_variables()

return global
