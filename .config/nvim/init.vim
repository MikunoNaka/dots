call plug#begin('~/.local/share/nvim/plugged') 

" find a better plugin to do this 
" which works with tsx and 
" doesn't interfere with coc-pairs
" Plug 'alvan/vim-closetag'

" basic
Plug 'itchyny/lightline.vim'
Plug 'Yggdroot/indentLine'
" Plug 'romgrk/doom-one.vim'
Plug 'MikunoNaka/doom-one.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" web dev
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'maxmellon/vim-jsx-pretty'
Plug 'yuezk/vim-js'

" Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
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
  \ 'coc-cpp',
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

" for tab stop
filetype plugin indent on
" On pressing tab, insert 2 spaces
set expandtab
" show existing tab with 2 spaces width
set tabstop=2
set softtabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2

" map leader to Space
let mapleader = " " 

" mouse support for visual, etc
set mouse=a

" copy/paste from clipboard
" Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P



" set filetypes as typescriptreact
" augroup SyntaxSettings
"     autocmd!
"     autocmd BufNewFile,BufRead *.tsx set filetype=typescript
" augroup END


" colors and theming (copied from DT's config)
"highlight LineNr           ctermfg=1    ctermbg=none    cterm=none

" disable word wrapping
set nowrap

" this makes firenvim work
" if exists('g:started_by_firenvim')
"   set laststatus=0
"   au BufEnter github.com_*.txt set filetype=markdown
" else
"   set laststatus=2
" endif

" firenvim config
" let g:firenvim_config = { 
"     \ 'globalSettings': {
"         \ 'alt': 'all',
"     \  },
"     \ 'localSettings': {
"         \ '.*': {
"             \ 'cmdline': 'neovim',
"             \ 'content': 'text',
"             \ 'priority': 0,
"             \ 'selector': 'textarea',
"             \ 'takeover': 'always',
"         \ },
"     \ }
" \ }
" let fc = g:firenvim_config['localSettings']
" let fc['.*'] = { 'selector': 'textarea:not([readonly]), div[role="textbox"]' }
" let fc['.*'] = { 'takeover': 'always' }
