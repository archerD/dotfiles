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
        vim.keymap.set('n', '<leader>fg', builtin.live_grep, {desc = "Telescope grep search"})
        vim.keymap.set('n', '<leader>fb', builtin.buffers, {desc = "Telescope buffers"})
        vim.keymap.set('n', '<leader>fh', builtin.help_tags, {desc = "Telescope help tags"})

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
                    "bash",
                },
                highlight = { enable = true },
                indent = { enable = true },
            })
        end,
        enabled=true },

    -- to set how many spaces a level of indentation is automatically.
    { 'NMAC427/guess-indent.nvim', opts = {}, enabled=true }, -- is faster
    { 'tpope/vim-sleuth', enabled=false }, -- is vim compatible, probably does more/smarter

    { "numToStr/Comment.nvim",
        opts = {},
    },

    { 'lewis6991/gitsigns.nvim',
        opts = {
            on_attach = function (bufnr)
                -- suggested configuration, emulates vim-gitgutter keymaps
                local gs = package.loaded.gitsigns

                local function map(mode, l, r, desc, opts)
                    opts = opts or {}
                    opts.buffer = bufnr
                    opts.desc = desc or ""
                    vim.keymap.set(mode, l, r, opts)
                end

                -- Navigation
                map('n', ']c', function()
                    if vim.wo.diff then return ']c' end
                    vim.schedule(function() gs.next_hunk() end)
                    return '<Ignore>'
                end, "Goto next change", { expr = true })

                map('n', '[c', function()
                    if vim.wo.diff then return '[c' end
                    vim.schedule(function() gs.prev_hunk() end)
                    return '<Ignore>'
                end, "Goto previous change", { expr = true })

                -- Actions
                map('n', '<leader>hs', gs.stage_hunk, "Stage hunk")
                map('n', '<leader>hr', gs.reset_hunk, "Reset hunk (discard change)")
                map('v', '<leader>hs', function() gs.stage_hunk { vim.fn.line('.'), vim.fn.line('v') } end, "Stage hunk")
                map('v', '<leader>hr', function() gs.reset_hunk { vim.fn.line('.'), vim.fn.line('v') } end, "Reset hunk")
                map('n', '<leader>hS', gs.stage_buffer, "Stage all hunks in buffer")
                map('n', '<leader>hu', gs.undo_stage_hunk, "Restore hunk (undo staging)")
                map('n', '<leader>hR', gs.reset_buffer, "Reset all hunks in buffer (discard ALL changes)")
                map('n', '<leader>hp', gs.preview_hunk, "Preview hunk")
                map('n', '<leader>hb', function() gs.blame_line { full = true } end, "Git blame on line")
                map('n', '<leader>tb', gs.toggle_current_line_blame, "Git blame inline")
                map('n', '<leader>hd', gs.diffthis, "Show a diff of the change")
                map('n', '<leader>hD', function() gs.diffthis('~') end, "Show a diff of the entire buffer changes")
                map('n', '<leader>td', gs.toggle_deleted, "Show old version inline")

                -- Text object
                map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
            end
        },
        enabled=true
    },

    -- need to explore and configure
    { 'gbprod/substitute.nvim', enabled=false },

    -- better highlighting and stuff on TODO comments and similar (FIX, HACK, WARN, PERF, NOTE, TEST)
    { "folke/todo-comments.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            signs = true,
            highlight = {
                pattern = {[[.*<(KEYWORDS):]], [[.*<(KEYWORDS)\(.+\):]]},
                keyword = "bg",
            },
            search = { pattern = [[\b(KEYWORDS)(\(.+\))?:]] },
        },
        init = function ()
            -- from todo-comments.nvim readme file.
            vim.keymap.set("n", "]t", function()
                require("todo-comments").jump_next()
            end, { desc = "Next todo comment" })

            vim.keymap.set("n", "[t", function()
                require("todo-comments").jump_prev()
            end, { desc = "Previous todo comment" })
        end,
        enabled=true },

    { "folke/trouble.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        opts = {},
        enabled=false }, -- a better list thing, could be worth keeping around.

    -- see nvim-treesitter/nvim-treesitter-context, wellle/context.vim for providing context to current line (i.e. function definition, etc.)
    -- see stevearc/aerial.nvim for providing an outline to current code

    { "folke/which-key.nvim", event = "VeryLazy",
      init = function()
        vim.o.timeout = true
        vim.o.timeoutlen = 1000
      end,
      opts = {
            presets = {
                operators = true, -- adds help for operators like d, y, ...
                motions = true, -- adds help for motions
                text_objects = true, -- help for text objects triggered after entering an operator
                windows = true, -- default bindings on <c-w>
                nav = true, -- misc bindings to work with windows
                z = true, -- bindings for folds, spelling and others prefixed with z
                g = true, -- bindings for prefixed with g
            },
        },
      enabled=true, }, -- primarily to help learn keybindings, but also useful for
                       -- marks, registers, spelling, and presets (motions, windows, etc.)
                       -- TODO: would be good to document some of the lsp keybindings

}

