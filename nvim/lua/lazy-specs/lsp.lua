local lsp_servers = function()
    -- Define the language servers.

    -- get the capabilities for completion
    local default_caps = vim.lsp.protocol.make_client_capabilities()
    local provided_cap = require('cmp_nvim_lsp').default_capabilities()
    local capabilities = vim.tbl_deep_extend('force', default_caps, provided_cap)
    --- wrapper for setting up and enabling language-server
    ---@param ls_name string server name
    ---@param config? vim.lsp.Config configuration overrides, capabilities are added as well
    local lspconfig = function(ls_name, config)
        -- could also change the config by making files in .dotfiles/nvim/after/lsp/
        -- each file would return the config instead of passing it to this function. i.e.
        --[[ ls_name.lua
            return config
        ]]
        config = config or {}
        -- there maybe other ways to do this, maybe by modifying vim.lsp.ClientConfig...
        config.capabilities = capabilities
        vim.lsp.config(ls_name, config)
        vim.lsp.enable(ls_name)
    end

    lspconfig('clangd')
    lspconfig('hls')
    lspconfig('gopls')
    lspconfig('rust_analyzer', {
        settings = {
            ['rust-analyzer'] = {},
        },
    })
    lspconfig('nixd', {
        settings = {
            nixd = {
                formatting = {
                    command = { "nixfmt" },
                },
            },
        },
    })
    lspconfig('lua_ls', {
        on_init = function(client)
            if client.workspace_folders then
                local path = client.workspace_folders[1].name
                if
                    path ~= vim.fn.stdpath('config')
                    and (vim.uv.fs_stat(path .. '/.luarc.json') or vim.uv.fs_stat(path .. '/.luarc.jsonc'))
                then
                    return
                end
            end

            client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
                runtime = {
                    -- Tell the language server which version of Lua you're using (most
                    -- likely LuaJIT in the case of Neovim)
                    version = 'LuaJIT',
                    -- Tell the language server how to find Lua modules same way as Neovim
                    -- (see `:h lua-module-load`)
                    path = {
                        'lua/?.lua',
                        'lua/?/init.lua',
                    },
                },
                -- Make the server aware of Neovim runtime files
                workspace = {
                    checkThirdParty = false,
                    library = {
                        vim.env.VIMRUNTIME
                        -- Depending on the usage, you might want to add additional paths
                        -- here.
                        -- '${3rd}/luv/library'
                        -- '${3rd}/busted/library'
                    }
                    -- Or pull in all of 'runtimepath'.
                    -- NOTE: this is a lot slower and will cause issues when working on
                    -- your own configuration.
                    -- See https://github.com/neovim/nvim-lspconfig/issues/3189
                    -- library = {
                    --   vim.api.nvim_get_runtime_file('', true),
                    -- }
                }
            })
        end,
        settings = {
            Lua = {}
        }
    })
end

return {
    -- language server and completion, based off builtin lsp support
    {"neovim/nvim-lspconfig",
      dependencies = { 'hrsh7th/cmp-nvim-lsp' },
      config = function()
        lsp_servers() -- define the lsp servers we use
        --- Setting up keybindings, based off https://github.com/neovim/nvim-lspconfig#suggested-configuration
        -- Global mappings.
        -- See `:help vim.diagnostic.*` for documentation on any of the below functions
        vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float, {desc="Show diagnostic"})
        vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, {desc="Previous diagnostic"})
        vim.keymap.set('n', ']d', vim.diagnostic.goto_next, {desc="Next diagnostic"})
        vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, {desc="Load diagnostics to location list"})

        -- Use LspAttach autocommand to only map the following keys
        -- after the language server attaches to the current buffer
        vim.api.nvim_create_autocmd('LspAttach', {
          group = vim.api.nvim_create_augroup('UserLspConfig', {}),
          callback = function(ev)
            -- Enable completion triggered by <c-x><c-o>
            vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

            -- Buffer local mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            local function opts (desc)
                return { buffer = ev.buf, desc="LSP: " .. desc, }
            end
            vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts("go to declaration"))
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts("go to definition"))
            vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts("go to implementation"))
            vim.keymap.set('n', '<leader>k', vim.lsp.buf.hover, opts("show lsp hover information"))
            vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts("show signature"))
            vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts("add workspace to lsp"))
            vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts("remove workspace from lsp"))
            vim.keymap.set('n', '<leader>wl', function()
              print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end, opts("list workspaces in lsp"))
            vim.keymap.set('n', '<leader>o', vim.lsp.buf.document_symbol, opts("open document outline"))
            vim.keymap.set('n', '<leader>t', vim.lsp.buf.type_definition, opts("show type"))
            vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts("rename symbol"))
            vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts("list code actions"))
            vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts("list references"))
            vim.keymap.set('v', '<leader>f', function()
              vim.lsp.buf.format { async = true }
            end, opts("format selection"))
          end,
        })

        --[[ old vimscript YouCompleteMe keybindings (https://github.com/ycm-core/YouCompleteMe)
        " <leader>d runs :YcmShowDetailedDiagnostic by default
        let g:ycm_key_detailed_diagnostics = '<leader>D'
        nmap <leader>d <plug>(YCMHover)
        " <C-Space> triggers the completion menu for semantic completion by default:
        "let g:ycm_key_invoke_completion = '<C-Space>'
        ]]
      end,
    },

    -- consider, for displaying lsp info on cursor
    { 'LukasPietzschmann/boo.nvim', enabled=false },

    -- snippets
    { 'L3MON4D3/LuaSnip',
        dependencies = {
            'rafamadriz/friendly-snippets',
            'saadparwaiz1/cmp_luasnip',
        },
        config = function ()
            require('luasnip.loaders.from_vscode').lazy_load()
        end,
        -- build = "make install_jsregexp"
    },
    -- auto complete
    { 'hrsh7th/cmp-nvim-lsp', config = true, },
    { 'hrsh7th/nvim-cmp',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
            -- 'hrsh7th/cmp-vsnip', 'hrsh7th/vim-vsnip', -- vsnip snippets
            'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip', -- luasnip snippets
            -- 'micangl/cmp-vimtex',
        },
        config = function()
            local cmp = require('cmp')
            local luasnip = require("luasnip")

            local has_words_before = function()
                unpack = unpack or table.unpack
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0 and
                    vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
            end

            cmp.setup({
                snippet = {
                    expand = function(args)
                        -- vim.fn["vsnip#anonymous"](args.body) -- vsnip snippets
                        require('luasnip').lsp_expand(args.body) -- luasnip snippets
                    end,
                },
                sources = cmp.config.sources({
                    { name = 'nvim_lsp' },
                    -- {name = 'vsnip'}, -- vsnip snippets
                    { name = 'luasnip' }, -- luasnip snippets
                    -- { name = 'vimtex' },
                }, {
                        { name = 'buffer' },
                    }),
                mapping = cmp.mapping.preset.insert({
                    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
                    ['<C-f>'] = cmp.mapping.scroll_docs(4),
                    ['<C-k>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    ['<C-j>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    ['<C-Space>'] = cmp.mapping.complete(), -- open the completion menu
                    ['<C-e>'] = cmp.mapping.abort(),
                    ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.

                    -- from https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings#super-tab-like-mapping
                    ["<Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_next_item()
                            -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
                            -- that way you will only jump inside the snippet region
                        elseif luasnip.expand_or_jumpable() then
                            luasnip.expand_or_jump()
                        elseif has_words_before() then
                            cmp.complete()
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                    ["<S-Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                            luasnip.jump(-1)
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                }),
            })
        end,
    },
}
