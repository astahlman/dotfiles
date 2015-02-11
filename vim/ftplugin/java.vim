set foldmethod=syntax
set foldlevelstart=1

" eclipse quick fix
noremap <C-f> :JavaCorrect<CR>
"eclipse organize imports
nnoremap <C-I><C-O> :JavaImportOrganize<CR>
nnoremap <buffer> <CR> :JavaSearchContext<CR>
" view references
nnoremap <buffer> <leader>r :FindReferences
command! -nargs=1 FindReferences call FindReferences(<f-args>)
function! FindReferences(target)
	:execute ":JavaSearch -p " . a:target . " -t all -x references -i"
endfunction

