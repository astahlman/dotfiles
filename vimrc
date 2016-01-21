set number
set nocompatible

set wildmenu " Autocomplete menu?

set incsearch
set hlsearch

set encoding=utf-8

set autochdir " make current directory relative to the open file

" more natural splitting
set splitbelow
set splitright

set showmatch " matching brackets
set matchtime=2 " show matches for 2 seconds
set number
set incsearch
set ignorecase
set smartcase
set hlsearch

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" for runtimepath management, install plugins to bundle
" breaks file type detection
"execute pathogen#infect()  
call pathogen#incubate()
filetype plugin on
filetype indent on
syntax on

" color scheme
colors evening

" map <leader> to ,
let mapleader = "," 

" paste replace word
nnoremap prw "_cw<C-R>"<Esc> 
" Ctrl-o opens NERDTree window
nnoremap <C-o> :NERDTree<CR>
" Ctrl-O+b name opens bookmark == name
nnoremap <C-o>b :NERDTreeFromBookmark 
" eclim organize imports
nnoremap <C-i><C-o> :JavaImportOrganize<CR>
" clear search highlighting
nnoremap <leader>/ :nohlsearch<CR>
" tab switching
nnoremap ,tn :tabnext<CR>
nnoremap ,tp :tabprev<CR>
" window switching
nnoremap ,wh :wincmd h<CR>
nnoremap ,wj :wincmd j<CR>
nnoremap ,wk :wincmd k<CR>
nnoremap ,wl :wincmd l<CR>

set clipboard=unnamed

" Lines added by the Vim-R-plugin command :RpluginConfig (2014-Oct-18 19:06):
" Press the space bar to send lines (in Normal mode) and selections to R:
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine

" Force Vim to use 256 colors if running in a capable terminal emulator:
if &term =~ "xterm" || &term =~ "256" || $DISPLAY != "" || $HAS_256_COLORS == "yes"
    set t_Co=256
endif

" There are hundreds of color schemes for Vim on the internet, but you can
" start with color schemes already installed.
" Click on GVim menu bar "Edit / Color scheme" to know the name of your
" preferred color scheme, then, remove the double quote (which is a comment
" character, like the # is for R language) and replace the value "not_defined"
" below:
"colorscheme not_defined

" Workaround for error in the C nailgun client
" See https://groups.google.com/forum/#!topic/eclim-user/MbxuE1i1n3s
let g:EclimNailgunClient='python'
