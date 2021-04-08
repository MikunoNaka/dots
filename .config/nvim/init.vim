call plug#begin('~/local/share/nvim/plugged') 

" find a better plugin to do this 
" which works with tsx and 
" doesn't interfere with coc-pairs
Plug 'alvan/vim-closetag'

Plug 'itchyny/lightline.vim'
Plug 'Yggdroot/indentLine'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'romgrk/doom-one.vim'
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'maxmellon/vim-jsx-pretty'
Plug 'yuezk/vim-js'


" Plug 'leafgarland/typescript-vim'
" Plug 'vim-airline/vim-airline'
" Plug 'dracula/vim', { 'as': 'dracula' }
" Plug 'sts10/vim-pink-moon'

call plug#end()

" closetag config
let g:closetag_filenames = '*.html,*.xhtml,*.phtml'
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx'
let g:closetag_filetypes = 'html,xhtml,phtml'
let g:closetag_xhtml_filetypes = 'xhtml,jsx'
let g:closetag_emptyTags_caseSensitive = 1
let g:closetag_regions = {
    \ 'typescript.tsx': 'jsxRegion,tsxRegion',
    \ 'javascript.jsx': 'jsxRegion',
    \ }
let g:closetag_shortcut = '>'
let g:closetag_close_shortcut = '<leader>>'

let g:indentLine_color_term = 239

" coc config
let g:coc_global_extentions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-eslint',
  \ 'coc-prettier',
  \ 'coc-go', 
  \ 'coc-json',
  \ 'coc-tsserver', 
  \ 'coc-css',
  \ 'coc-html',
  \ ]

" line numbers
set number
set nu

" colorscheme config
colorscheme doom-one
set termguicolors
syntax enable

" remove vim status because lightline replaces it
set noshowmode

" lightline config
" colorscheme wombat for lightline
let g:lightline = {
      \ 'colorscheme': 'darcula'
      \ }

" hexokinase config
let g:Hexokinase_highlighters = ['virtual']

" jsx-pretty config
let g:vim_jsx_pretty_highlight_close_tag = 1
let g:vim_jsx_pretty_colorful_config = 1

" set filetypes as typescriptreact
" augroup SyntaxSettings
"     autocmd!
"     autocmd BufNewFile,BufRead *.tsx set filetype=typescript
" augroup END


" colors and theming (copied from DT's config)
highlight LineNr           ctermfg=1    ctermbg=none    cterm=none
