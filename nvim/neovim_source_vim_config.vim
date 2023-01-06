" link to ~/.config/nvim/init.vim to use.
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.dotfiles/vim/source-all.vim

