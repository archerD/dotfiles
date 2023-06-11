-- settings
require('keybindings')
require('settings')
if vim.g.neovide then -- for neovide only...
    require('gui')
end
-- plugins
require('plugins')
require('lsp')
require('plugin-settings')

