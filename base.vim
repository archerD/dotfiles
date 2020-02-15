""" These options relate to how long lines wrap
"set linebreak showbreak=+
"
"""" These will make trailing spaces show up as '-' characeters
"set list listchars=trail:-

" Some settings from vim.fandom.com/wiki/Example_vimrc
" Enable modern Vim features not compatible with Vi spec.
set nocompatible

" highlight all matches as you type
set incsearch
set hlsearch
" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase

" Allow backspacing over autoindent, linebreaks and start of insert action
set backspace=indent,eol,start


" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>
" Modify Y behavior to align with C and D behavior
nnoremap Y y$
" Changes K from open help to split line at cursor (mirrors J[oin])
nnoremap K i<Cr><Esc>


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

