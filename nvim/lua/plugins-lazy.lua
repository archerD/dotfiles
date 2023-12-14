-- Using lazy.nvim as a plugin manager (from the README)
--  Plugins are managed with the :Lazy command. Open the help with <?> to see all the key mappings.
--  You can press <CR> on a plugin to show its details. Most properties can be hovered with <K> to open links, help files, readmes, git commits and git issues.
--  run `:checkhealth lazy` to check the install


-- bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- plugins to be managed
local plugins = {
    -- tpope plugins --
    -- allow . to repeat some plugin actions
    {"tpope/vim-repeat"},
    -- do stuff with surrounding brackets and such
    {"tpope/vim-surround"},
    -- git integration
    {"tpope/vim-fugitive"},
    -- allow saving vim state
    {"tpope/vim-obsession"},
    -- easy commenting
    {"tpope/vim-commentary"},

    -- plugin for text object support
    -- see https://github.com/kana/vim-textobj-user/wiki for plugins using this
    {"kana/vim-textobj-user"},
    -- add az and iz text objects for folds
    {"kana/vim-textobj-fold"},
    -- add af and if text objects for functions
    {"kana/vim-textobj-function", enabled=false},

    -- matching brackets or other stuff
    {"kana/vim-smartinput", enabled=false},
    -- allows I and A to be used in all visual modes to get the effect of I and A
    -- in visual block mode, in a nice way
    {"kana/vim-niceblock", enabled=false},
    -- Adding git hunk details to the sidebar
    {"airblade/vim-gitgutter"},

    -- local vimrc files for per project config
    {"embear/vim-localvimrc"},
    -- undo tree visualization
    {"mbbill/undotree"},

    -- full featured vim plugin
    {"lervag/vimtex"},

    -- plugins to consider
    {"nvim-lualine/lualine.nvim", enabled=false},
    {"frazrepo/vim-rainbow", enabled=false},

    -- language server and completion, based off builtin lsp support
    {"neovim/nvim-lspconfig"},
}

-- options for lazy.nvim
local opts = {}

-- Load lazy.nvim
require("lazy").setup(plugins, opts)

-- uninstall instructions:
-- remove the following files/directories:
-- * data: ~/.local/share/nvim/lazy
-- * state: ~/.local/state/nvim/lazy
-- * lockfile: ~/.config/nvim/lazy-lock.json

