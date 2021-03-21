call plug#begin('~/local/share/nvim/plugged')

" Plug 'vim-airline/vim-airline'
Plug 'itchyny/lightline.vim'
Plug 'alvan/vim-closetag'
Plug 'Yggdroot/indentLine'
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
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

" enabling the theme
" colorscheme dracula

" line numbers
:set number
:set nu

" remove vim status because lightline replaces it
set noshowmode

" lightline config
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }
