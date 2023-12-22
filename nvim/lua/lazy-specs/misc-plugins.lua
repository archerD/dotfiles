return {
    -- set vim.ui.select to use telescope, e.g. code actions use telescope
    { 'nvim-telescope/telescope-ui-select.nvim', },
    { 'nvim-telescope/telescope.nvim', branch = '0.1.x',
      config = function()
        require("telescope").setup({
            extensions = {
              ['ui-select'] = { require("telescope.themes").get_dropdown{} }
            }
        })
        -- telescope settings, many other searches possible.
        local builtin = require('telescope.builtin')
            -- TODO: add the desc key to all keymaps.
        vim.keymap.set('n', '<leader>ff', builtin.find_files, {desc = "Telescope file finder"})
        vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
        vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
        vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

        require("telescope").load_extension("ui-select")
      end,
      dependencies = { 'nvim-lua/plenary.nvim' } },

    { 'nvim-treesitter/nvim-treesitter', build = ":TSUpdate",
        config = function()
            local configs = require("nvim-treesitter.configs")
            configs.setup({
                ensure_installed = {
                    "lua",
                    "haskell",
                    "ocaml",
                    "latex",
                    "vimdoc",
                    "vim",
                    "nix",
                    "python",
                    "markdown",
                    "markdown_inline",
                },

                highlight = {enable=true},
                indent = {enable=true},
            })
        end,
        enabled=true },

    { "numToStr/Comment.nvim",
        opts = {},
    },

    { "folke/todo-comments.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {}, enabled=false }, -- better highlighting and stuff on TODO comments and other similar comments
                                    -- needs further investigation...

    -- see nvim-treesitter/nvim-treesitter-context, wellle/context.vim for providing context to current line (i.e. function definition, etc.)
    -- see stevearc/aerial.nvim for providing an outline to current code

    { "folke/which-key.nvim", event = "VeryLazy",
      init = function()
        vim.o.timeout = true
        vim.o.timeoutlen = 1000
      end,
      opts = { triggers_nowait = { "z=" }, },
      enabled=true, }, -- primarily to help learn keybindings, but also useful for
                       -- marks, registers, spelling, and presets (motions, windows, etc.)
                       -- TODO: would be good to document some of the lsp keybindings

}

