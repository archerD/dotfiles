" for YouCompleteMe in docker
set encoding=utf-8 
" add fugitive status line to default status line
set statusline=%<%f\ %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%)\ %P
" Add a language server for Haskell
let g:ycm_language_server = [
  \   { 'name': 'haskell',
  \     'filetypes': [ 'haskell', 'hs', 'lhs' ],
  \     'cmdline': [ 'hie-wrapper', '--lsp' ],
  \     'project_root_files': [ '.stack.yaml', 'cabal.config', 'package.yaml' ]
  \   }
  \ ]

" add the problem and solution latex environments to the list of things
" folded by vim-latex
"let g:Tex_FoldedEnvironments = 'solution,'
" Let ycm use vimtex to perform autocomplete in tex files
if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif
" get rid of warning for vimtex tex flavor
let g:tex_flavor = 'latex'

au VimEnter * let g:ycm_semantic_triggers.tex=g:vimtex#re#youcompleteme
" set completions from vimtex to include closing brace
let g:vimtex_complete_close_braces = 1
" set the directory for vimtex to put the build files
let g:vimtex_compiler_latexmk = {
    \ 'build_dir' : './vimtex-output',
    \}

" cmus controls
nnoremap <leader>i :CmusCurrent<cr>
nnoremap <leader>z :CmusPrevious<cr>
nnoremap <leader>x :CmusPlay<cr>
nnoremap <leader>c :CmusPause<cr>
nnoremap <leader>v :CmusStop<cr>
nnoremap <leader>b :CmusNext<cr>

