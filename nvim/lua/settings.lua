-- settings --

-- helpers --
-- local g = vim.g -- global variables
local o = vim.o -- options
local opt = vim.opt -- options with set/list support

-- neovide gui stuff, split into separate file if more settings are applied.
if vim.g.neovide then
    vim.o.guifont = "JetBrainsMono Nerd Font:h12"
end

-- colorschemes --
-- vim.cmd.colorscheme('industry')
--vim.cmd.colorscheme('slate-old')
-- currently set in plugins-lazy.lua, using carbonfox.
-- vim.cmd.colorscheme('carbonfox')


-- miscellaneous settings? --
-- Attempt to determine the type of a file based on its name and possibly its
-- contents. Use this to allow intelligent auto-indenting for each filetype,
-- and for plugins that are filetype specific.
vim.api.nvim_command('filetype indent plugin on')
-- Enable syntax highlighting
vim.api.nvim_command('syntax enable')

-- see :help nvim-defaults for changed defaults

-- a thing... --
-- how long until the swap file is (re)written
o.updatetime = 1000
-- menu settings
o.completeopt = 'menuone,noselect,longest' -- consider adding 'preview' to the list

-- search settings --
-- highlight all matches as you type
o.incsearch = true
o.hlsearch = true
-- use case insensitive search, except when using capital letters
o.ignorecase = true
o.smartcase = true


-- tab control --
-- These options relate to indenting. Most files are created with
-- tabstops set to 8 characters, so we won't touch 'tabstop
o.expandtab = true
o.shiftwidth = 4
o.softtabstop = 4
-- auto expand tabs
o.autoindent = false
o.smartindent = false -- use tree-sitter.ident instead


-- control settings --
-- enable mouse support in all modes
o.mouse = 'a'
o.mousemodel = 'extend'

-- Allow backspacing over autoindent, linebreaks and start of insert action (default)
opt.backspace = { 'indent', 'eol', 'start' }


-- visual settings --
-- set line numbers
o.number = true
o.relativenumber = true
-- better colors
o.termguicolors = true

-- adds the status bar (this is all the defaults in neovim)
o.ruler = true
o.laststatus = 2
o.showcmd = true
o.wildmenu = true

-- ensures that there is context around the current line
o.scrolloff = 2
-- wrap at words instead of characters
o.linebreak = true

