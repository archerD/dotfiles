""" These options relate to how long lines wrap
"set linebreak showbreak=+
"
"""" These will make trailing spaces show up as '-' characeters
"set list listchars=trail:-

" Some settings from vim.fandom.com/wiki/Example_vimrc
" Enable modern Vim features not compatible with Vi spec.
set nocompatible

" Set the mapleader and the local map leader to ' '(space bar)
let mapleader = " "
let maplocalleader = " "

" highlight all matches as you type
set incsearch
set hlsearch
" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase

" Allow backspacing over autoindent, linebreaks and start of insert action
set backspace=indent,eol,start

" Set the mapleader and the local map leader to ' '(space bar)
let mapleader = " "
let maplocalleader = " "

" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>
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

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on

" colorscheme, using slate for dark, and morning for light mode on gui.
if has('gui_running')
    colorscheme morning
else
    colorscheme slate
endif
" Enable syntax highlighting
syntax enable
set autoindent
"auto expand tabs
""" These options relate to the size of indenting. Most files are created with
""" tabstops set to 8 characters, so we won't touch 'tabstop'.
set expandtab
set shiftwidth=4
set softtabstop=4

" set line numbers
set number
set relativenumber

" adds the status bar
set ruler
set laststatus=2
set showcmd
set wildmenu

" ensures that there is context around the current line
set scrolloff=2
" wrap at words instead of characters
set linebreak

