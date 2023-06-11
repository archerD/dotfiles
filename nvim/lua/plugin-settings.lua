-- add fugitive status line to default status line
vim.o.statusline = '%<%f %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%) %P'

-- attempt at setting up comments for c and cpp files
vim.cmd([[ autocmd FileType c,cpp  let b:commentary_format='//%s' ]])
vim.cmd([[ autocmd FileType factor let b:commentary_format='! %s' ]])

-- get rid of warning for vimtex tex flavor
vim.g.tex_flavor = 'latex'

-- set completions from vimtex to include closing brace
vim.g.vimtex_complete_close_braces = 1
-- set the directory for vimtex to put the build files
vim.g.vimtex_compiler_latexmk = { build_dir = './vimtex-output' }
-- set the default viewer for vimtex
vim.g.vimtex_view_general_viewer = 'evince'

--[[ YouCompleteMe configuration
" Let ycm use vimtex to perform autocomplete in tex files
if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif
au VimEnter * let g:ycm_semantic_triggers.tex=g:vimtex#re#youcompleteme

" for YouCompleteMe in docker
set encoding=utf-8 
" Add a language server for Haskell
let g:ycm_language_server = [
  \   { 'name': 'haskell',
  \     'filetypes': [ 'haskell', 'hs', 'lhs' ],
  \     'cmdline': [ 'haskell-language-server-wrapper', '--lsp' ],
  \     'project_root_files': [ '*.cabal', 'stack.yaml', '.stack.yaml', 'cabal.config', 'cabal.project', 'package.yaml', 'hie.yaml' ]
  \   },
  \   { 'name': 'ocaml',
  \     'filetypes': [ 'ocaml', 'ml', 'mli' ],
  \     'cmdline': [ 'ocamllsp' ]
  \   }
  \ ]

" YouCompleteMe keybindings
" <leader>d runs :YcmShowDetailedDiagnostic by default
let g:ycm_key_detailed_diagnostics = '<leader>D'
nmap <leader>d <plug>(YCMHover)
" <C-Space> triggers the completion menu for semantic completion by default:
"let g:ycm_key_invoke_completion = '<C-Space>'
]]

