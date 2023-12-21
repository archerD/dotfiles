return {
    -- language server and completion, based off builtin lsp support
    {"neovim/nvim-lspconfig",
      config = function()
        -- Define the language servers.
        local lspconfig = require('lspconfig')
        lspconfig.hls.setup {}
        lspconfig.rust_analyzer.setup {
          -- Server-specific settings. See `:help lspconfig-setup`
          settings = {
            ['rust-analyzer'] = {},
          },
        }
        lspconfig.nixd.setup{}
        lspconfig.lua_ls.setup{ -- recommended setup for neovim lua from nvim-lspconfig
          on_init = function(client)
            local path = client.workspace_folders[1].name
            if not vim.loop.fs_stat(path..'/.luarc.json') and not vim.loop.fs_stat(path..'/.luarc.jsonc') then
              client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
                Lua = {
                  runtime = {
                    -- Tell the language server which version of Lua you're using
                    -- (most likely LuaJIT in the case of Neovim)
                    version = 'LuaJIT'
                  },
                  -- Make the server aware of Neovim runtime files
                  workspace = {
                    checkThirdParty = false,
                    library = {
                      vim.env.VIMRUNTIME
                      -- "${3rd}/luv/library"
                      -- "${3rd}/busted/library",
                    }
                    -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
                    -- library = vim.api.nvim_get_runtime_file("", true)
                  }
                }
              })

              client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
            end
            return true
          end
        }

        --- Setting up keybindings, based off https://github.com/neovim/nvim-lspconfig#suggested-configuration
        -- Global mappings.
        -- See `:help vim.diagnostic.*` for documentation on any of the below functions
        vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float)
        vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
        vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
        vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

        -- Use LspAttach autocommand to only map the following keys
        -- after the language server attaches to the current buffer
        vim.api.nvim_create_autocmd('LspAttach', {
          group = vim.api.nvim_create_augroup('UserLspConfig', {}),
          callback = function(ev)
            -- Enable completion triggered by <c-x><c-o>
            vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

            -- Buffer local mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            local opts = { buffer = ev.buf }
            vim.keymap.set('i', '<C-<space>>', '<C-X> <C-O>', opts) -- remap the omnifunction to ctrl space
            vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
            vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
            vim.keymap.set('n', '<leader>k', vim.lsp.buf.hover, opts)
            vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
            vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
            vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
            vim.keymap.set('n', '<leader>wl', function()
              print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end, opts)
            vim.keymap.set('n', '<leader>t', vim.lsp.buf.type_definition, opts)
            vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
            vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts)
            vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
            vim.keymap.set('v', '<leader>f', function()
              vim.lsp.buf.format { async = true }
            end, opts)
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

}
