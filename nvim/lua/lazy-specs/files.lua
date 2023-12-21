return { -- TODO: remove one of the file trees
         -- leaning to keeping neo-tree.nvim and deleting nvim-tree.lua and maybe oil.nvim...
    {
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v3.x",
        dependencies = {
          "nvim-lua/plenary.nvim",
          "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
          "MunifTanjim/nui.nvim",
          -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
        },
        opts = {
            filesystem = {
                use_libuv_file_watcher = true,
            },
        },
        vim.keymap.set('n', '<leader>ft', ':Neotree reveal<cr>', {})
    },
    { "stevearc/oil.nvim",
      opts = {},
      config = function(_, opts)
          require('oil').setup(opts)
          vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
      end,
      -- Optional dependencies
      dependencies = { "nvim-tree/nvim-web-devicons" },
    },

    {
      "nvim-tree/nvim-tree.lua",
      version = "*",
      lazy = false,
      dependencies = {
        "nvim-tree/nvim-web-devicons",
      },
      config = function()
        require("nvim-tree").setup {}
      end,
    },
}
