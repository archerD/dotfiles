-- settings --
vim.cmd [[colorscheme slate]]

--[[
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

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on

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

" enable mouse support in all modes
set mouse=a
set mousemodel=extend
]]


