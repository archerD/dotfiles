-- keybindings --
local function map(mode, keybind, value)
    vim.keymap.set(mode, keybind, value, { silent = true });
end

local function nmap(k, v)
    map('n', k, v)
end

-- swap the f/t repeat search keys
nmap(';', ',')
nmap(',', ';')

--[[
" Set the mapleader and the local map leader to ' '(space bar)
let mapleader = " "
let maplocalleader = " "

" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search, fix syntax highlighting, and update diffs
nnoremap <C-L> :nohl<CR>:diffupdate<CR>:syntax sync fromstart<CR><C-L>
" Modify Y behavior to align with C and D behavior (defaults to yy behavior)
nnoremap Y y$
" Changes K from open help to split line at cursor (mirrors J[oin])
nnoremap K i<Cr><Esc>
" and remap default K functionality (open help) to mapleader K or mapleader k
nnoremap <leader>K K
nnoremap <leader>k K

" gq and gw originally mapped to a reformatting keybind.
" gW wasn't mapped, and gQ was a special way to enter Ex mode.
" these mappings change gq(gQ) and gw(gW) to quit and write (by force), respectively.
nnoremap gq <C-W>q
nnoremap gQ <C-W>:q!
" testing: a mapping for gw to :w
nnoremap gw :w
nnoremap gW :w!

" map :W to :w, due to typos (:W is nothing).
command! W w
]]

