-- keybindings --

-- helper functions --
local function map(mode, keybind, value)
    vim.keymap.set(mode, keybind, value, { silent = true });
end

local function nmap(k, v)
    map('n', k, v)
end

-- set leader keys
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- swap the f/t repeat search keys
nmap(';', ',')
nmap(',', ';')

-- Map <C-L> (redraw screen) to also turn off search highlighting until the
-- next search, fix syntax highlighting, and update diffs
nmap('<C-L>', ':nohl<CR>:diffupdate<CR>:syntax sync fromstart<CR><C-L>')

-- Changes K from open help to split line at cursor (mirrors J[oin])
nmap('K', 'i<Cr><Esc>')
-- and remap default K functionality (open help) to mapleader K or mapleader k
vim.o.keywordprg = ':help'
nmap('<leader>k', 'K')
nmap('<leader>K', 'K')

-- gq and gw originally mapped to a reformatting keybind.
-- gW wasn't mapped, and gQ was a special way to enter Ex mode.
-- these mappings change gq(gQ) and gw(gW) to quit and write (by force), respectively.
nmap('gq', '<C-W>q')
nmap('gQ', '<C-W>:q!')
-- testing: a mapping for gw to :w
nmap('gw', ':w<Cr>')
nmap('gW', ':w!')

--[[ still need to figure this out...
" map :W to :w, due to typos (:W is nothing).
command! W w
]]

