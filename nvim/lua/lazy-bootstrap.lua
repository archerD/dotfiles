-- Using lazy.nvim as a plugin manager, from the README:
--  Plugins are managed with the :Lazy command. Open the help with <?> to see all the key mappings.
--  You can press <CR> on a plugin to show its details. Most properties can be hovered with <K> to open links, help files, readmes, git commits and git issues.
--  run `:checkhealth lazy` to check the install

-- TODO: Consider integrating b-src/lazy-nix-helper.nvim, to let nix handle plugin installation, while lazy handles loading/configuration.

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

-- options for lazy.nvim
local opts = { install = { colorscheme = {"carbonfox", "slate-old", "industry"} } }
-- Load lazy.nvim, with plugin specification in the lazy-specs/ folder
require("lazy").setup("lazy-specs", opts)

-- uninstall instructions:
-- remove the following files/directories:
-- * data: ~/.local/share/nvim/lazy
-- * state: ~/.local/state/nvim/lazy
-- * lockfile: ~/.config/nvim/lazy-lock.json

