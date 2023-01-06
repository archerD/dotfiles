" requires Vundle to be installed, to install, run the following command:
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" allow . to repeat some plugin actions
Plugin 'tpope/vim-repeat'
" do stuff with surrounding brackets and such
Plugin 'tpope/vim-surround'
" git integration
Plugin 'tpope/vim-fugitive'
" allow saving vim state
Plugin 'tpope/vim-obsession'
" easy commenting
Plugin 'tpope/vim-commentary'

" undo tree visualization
Plugin 'mbbill/undotree'

" plugin for text object support
" see https://github.com/kana/vim-textobj-user/wiki for plugins using this
Plugin 'kana/vim-textobj-user'
" add az and iz text objects for folds
Plugin 'kana/vim-textobj-fold'
" add af and if text objects for functions
"Plugin 'kana/vim-textobj-function'

" matching brackets or other stuff
"Plugin 'kana/vim-smartinput'
" allows I and A to be used in all visual modes to get the effect of I and A
" in visual block mode, in a nice way
"Plugin 'kana/vim-niceblock'
" Adding git hunk details to the sidebar
Plugin 'airblade/vim-gitgutter'

" local vimrc files for per project config
Plugin 'embear/vim-localvimrc'

" full featured vim plugin
Plugin 'lervag/vimtex'

" language server and completion thing...
" YCM has a compiled component, for install direction, see
" https://github.com/ycm-core/YouCompleteMe#installation
Plugin 'ycm-core/YouCompleteMe'

" Factor plugin, if the factor repository is already downloaded, space can be
" saved by symlinking .vim/bundle/factor to the existing repo before install.
Plugin 'factor/factor', { 'rtp': 'misc/vim' }

" Coq plugin.
Plugin 'whonore/Coqtail'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

