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

