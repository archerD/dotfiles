-- Using Packer as a plugin manager

--[[ Packer Usage guide
-- You must run this or `PackerSync` whenever you make changes to your plugin configuration
-- Regenerate compiled loader file
:PackerCompile

-- Remove any disabled or unused plugins
:PackerClean

-- Clean, then install missing plugins
:PackerInstall

-- Clean, then update and install plugins
-- supports the `--preview` flag as an optional first argument to preview updates
:PackerUpdate

-- Perform `PackerUpdate` and then `PackerCompile`
-- supports the `--preview` flag as an optional first argument to preview updates
:PackerSync

-- Show list of installed plugins
:PackerStatus

-- Loads opt plugin immediately
:PackerLoad completion-nvim ale
]]

-- function to bootstrap packer installation.
local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

-- Reload neovim when plugins.lua (this file) is saved.
vim.cmd([[
    augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerCompile
    augroup end
]])

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- tpope plugins --
    -- allow . to repeat some plugin actions
    use 'tpope/vim-repeat'
    -- do stuff with surrounding brackets and such
    use 'tpope/vim-surround'
    -- git integration
    use 'tpope/vim-fugitive'
    -- allow saving vim state
    use 'tpope/vim-obsession'
    -- easy commenting
    use 'tpope/vim-commentary'

    -- plugin for text object support
    -- see https://github.com/kana/vim-textobj-user/wiki for plugins using this
    use 'kana/vim-textobj-user'
    -- add az and iz text objects for folds
    use 'kana/vim-textobj-fold'
    -- add af and if text objects for functions
    --use 'kana/vim-textobj-function'

    -- matching brackets or other stuff
    --use 'kana/vim-smartinput'
    -- allows I and A to be used in all visual modes to get the effect of I and A
    -- in visual block mode, in a nice way
    --use 'kana/vim-niceblock'
    -- Adding git hunk details to the sidebar
    use 'airblade/vim-gitgutter'

    -- local vimrc files for per project config
    use 'embear/vim-localvimrc'
    -- undo tree visualization
    use 'mbbill/undotree'

    -- full featured vim plugin
    use 'lervag/vimtex'

    -- Coq plugin.
    --use 'whonore/Coqtail'

    -- plugins to consider
    --use nvim-lualine/lualine.nvim
    --use frazrepo/vim-rainbow

    -- language server and completion thing... need to use builtin support instead
    -- YCM has a compiled component, for install direction, see
    -- https://github.com/ycm-core/YouCompleteMe#installation
    --use 'ycm-core/YouCompleteMe'
    -- see the following for lsp support
    use 'neovim/nvim-lspconfig'

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)

