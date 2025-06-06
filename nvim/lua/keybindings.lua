-- keybindings --

-- helper functions --
local function map(mode, keybind, value, desc)
    vim.keymap.set(mode, keybind, value, { silent = true, desc = desc,});
end

local function nmap(k, v, d)
    map('n', k, v, d)
end
local function imap(k, v, d)
    map('i', k, v, d)
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
imap('<C-L>', '<c-g>u<Esc>[s1z=gi<c-g>u', "fix last spelling mistake")

-- Changes K from open help to split line at cursor (mirrors J[oin])
nmap('K', 'i<Cr><Esc>')
-- and remap default K functionality (open help) to mapleader K or mapleader k
vim.o.keywordprg = ':help'
nmap('<leader>k', 'K', 'Open help menu')
nmap('<leader>K', 'K', 'Open help menu')

-- Automatically add semicolon or comma at the end of the line in INSERT and NORMAL modes
imap(";;", "<ESC>A;")
imap(",,", "<ESC>A,")
nmap(";;", "A;<ESC>")
nmap(",,", "A,<ESC>")
-- yank count lines, comment them out, then paste them again below.
vim.keymap.set("n", "ycc", function()
    return 'yy' .. vim.v.count1 .. "gcc']p"
end, { remap = true, expr = true, desc = "Duplicate and comment count lines" })

-- gq and gw originally mapped to a reformatting keybind.
-- gW wasn't mapped, and gQ was a special way to enter Ex mode.
-- these mappings change gq(gQ) and gw(gW) to quit and write (by force), respectively.
nmap('gq', '<C-W>q', 'Quit window')
nmap('gQ', '<C-W>:q!', 'Force quit window')
-- testing: a mapping for gw to :w
nmap('gw', ':w<Cr>', 'Write buffer')
nmap('gW', ':w!', 'Force write buffer')

-- terminal stuff
nmap('<leader>T', ':split +term<CR>', 'open a terminal in a new window')
map('t', '<Esc>', '<C-\\><C-n>', 'Easy way to exit terminal mode')

--[[ still need to figure this out...
" map :W to :w, due to typos (:W is nothing).
command! W w
]]

