return {
    -- status line plugin
    { "nvim-lualine/lualine.nvim",
      dependencies = { "nvim-tree/nvim-web-devicons" },
      opts = {
          -- sections = { lualine_b = { 'FugitiveHead' } }
          extensions = {
              'fugitive',
              -- 'lazy',
              'neo-tree',
              'quickfix',
              'oil',
          },
      },
      enabled=true },

    -- visual improvement on showing indents
    { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {indent={char="▏"}} },

    { "rcarriga/nvim-notify",
        opts = {
            timeout = 2000,
        },
        init = function(_)
            vim.notify = require("notify")
        end,
        config = function(_, opts)
            require("notify").setup(opts)
        end,
        lazy = true,
    }, -- not clear on what uses this, but it looks nice...

    -- colorschemes! (carbonfox looks good for now, but no other ordering implied)
    { "EdenEast/nightfox.nvim", lazy = false, priority = 1000,
        -- use :NightfoxInteractive to see changes on write
        opts = {
            specs = {
                carbonfox = {
                    -- brightening the visual selection slightly for better visibility
                    --sel0 = "#303030", -- defaults to #2a2a2a, this had insufficent contrast for me.
                    -- changing the selections for better visibility of visual mode selection (using values from terafox)
                    --sel0 = "#293e40", -- defaults to #2a2a2a, this had insufficent contrast for me.
                    --sel1 = "#425e5e", -- defaults to #525253
                    -- inverting bg0 and bg1
                    -- bg1  = "#0c0c0c", --default bg
                    -- bg0  = "#161616", -- statusline and float
                    bg1  = "bg0", --default bg
                    bg0  = "bg1", -- statusline and float
                    syntax = { -- experiment with making comments yellow instead of gray
                        -- comment = "#f5e8aa" -- butter
                        -- comment = "#ffed5f" -- canary yellow
                        -- comment = "#d2b55b" -- trombone
                        -- comment = "#e2ca76" -- sand
                        comment = "#f8de7e" -- mellow yellow
                    },
                },
            },
            groups = {
                all = {
                    -- to show the end of file more cleanly.
                    EndOfBuffer = { fg = "bg4", bg = "black" },
                },
            },
            options = {
                dim_inactive = false, -- maybe??
                styles = {
                    -- comments = "italic",
                },
                inverse = {
                    match_paren = true,
                    visual = false, -- alternative solution to visual selection contrast problem
                },
            },
        },
        config = function(_, opts)
            require("nightfox").setup(opts)
            vim.cmd.colorscheme("carbonfox")
        end,
    }, -- :colorscheme carbonfox
    { "Mofiqul/vscode.nvim" }, -- :colorscheme vscode or require('vscode').load()
    { "bluz71/vim-moonfly-colors", name = "moonfly", lazy = true, }, -- :colorscheme moonfly
    { "dasupradyumna/midnight.nvim", lazy = true, }, -- :colorscheme midnight
    { "kartikp10/noctis.nvim", lazy = true, dependencies={"rktjmp/lush.nvim"}, }, -- :colorscheme noctis
    { "rodnaph/vim-color-schemes", enabled=false }, -- a large collection of schemes...
                                                    -- see https://vimcolorschemes.com/rodnaph/vim-color-schemes

}
