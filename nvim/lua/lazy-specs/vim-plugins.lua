return { -- handful of plugins from vim, mostly better keybindings.
    -- tpope plugins --
    -- allow . to repeat some plugin actions
    {"tpope/vim-repeat"},
    -- do stuff with surrounding brackets and such
    {"tpope/vim-surround"},
    -- git integration
    {"tpope/vim-fugitive",
        config = function()
            -- add fugitive status line to default status line
            -- vim.o.statusline = '%<%f %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%) %P'
        end,
    },
    -- allow saving vim state
    {"tpope/vim-obsession"}, -- see folke/persistence.nvim as possible alternative
    -- easy commenting
    {"tpope/vim-commentary",
        config = function()
            -- attempt at setting up comments for c and cpp files
            vim.cmd([[ autocmd FileType c,cpp  let b:commentary_format='//%s' ]])
            vim.cmd([[ autocmd FileType factor let b:commentary_format='! %s' ]])
        end,
    }, -- see numToStr/comment.nvim as possible alternative

    -- plugins for text object support
    -- see https://github.com/kana/vim-textobj-user/wiki for more plugins using kana/vim-textobj-user
    -- add az and iz text objects for folds
    {"kana/vim-textobj-fold", dependencies={"kana/vim-textobj-user"}},
    -- add af and if text objects for functions
    {"kana/vim-textobj-function", dependencies={"kana/vim-textobj-user"}, enabled=false},

    -- matching brackets or other stuff
    {"kana/vim-smartinput", enabled=false},
    -- allows I and A to be used in all visual modes to get the effect of I and A
    -- in visual block mode, in a nice way
    {"kana/vim-niceblock", enabled=false},
    -- Adding git hunk details to the sidebar
    {"airblade/vim-gitgutter"}, -- see lewis6991/gitsigns.nvim as possible alternative

    -- local vimrc files for per project config
    {"embear/vim-localvimrc"},
    -- undo tree visualization
    {"mbbill/undotree"},

    -- full featured vim plugin
    {"lervag/vimtex", -- TODO: consider moving to different file
        config = function()
            -- get rid of warning for vimtex tex flavor
            -- vim.g.tex_flavor = 'latex'

            -- set completions from vimtex to include closing brace
            vim.g.vimtex_complete_close_braces = 1
            -- set the directory for vimtex to put the build files
            vim.g.vimtex_compiler_latexmk = { build_dir = './vimtex-output' }
            -- set the default viewer for vimtex
            vim.g.vimtex_view_general_viewer = 'evince'
        end,
    },
}
