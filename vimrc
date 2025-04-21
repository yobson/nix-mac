filetype plugin on
syntax on
filetype plugin indent on
set ttyfast
set number
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set splitright
set autoindent
set nowrap
set nocompatible 
set modeline

au BufRead,BufNewFile *.agda call AgdaFiletype()
au BufWritePost *.agda execute "normal! :CornelisLoad\<CR>"
au QuitPre *.agda :CornelisCloseInfoWindows
function! AgdaFiletype()
    nnoremap <buffer> <C-c><C-l> :CornelisLoad<CR>
    nnoremap <buffer> <C-c><C-r> :CornelisRefine<CR>
    nnoremap <buffer> <C-c><C-g> :CornelisGive<CR>
    nnoremap <buffer> <C-c><C-c> :CornelisMakeCase<CR>
    nnoremap <buffer> <C-c><C-,> :CornelisTypeContext Simplified<CR>
    nnoremap <buffer> <C-c><C-.> :CornelisTypeContextInfer<CR>
    nnoremap <buffer> <C-c><C-s> :CornelisSolve<CR>
    nnoremap <buffer> <C-c><C-a> :CornelisAuto<CR>
    nnoremap <buffer> <C-c><C-?> :CornelisQuestionToMeta<CR>
    nnoremap <buffer> gd         :CornelisGoToDefinition<CR>
    nnoremap <buffer> <C-c><C-b> :CornelisPrevGoal<CR>
    nnoremap <buffer> <C-c><C-f> :CornelisNextGoal<CR>
    nnoremap <buffer> <C-A>     :CornelisInc<CR>
    nnoremap <buffer> <C-X>     :CornelisDec<CR>

      " Highlight holes with a yellow undercurl/underline:
    highlight CornelisHole ctermfg=yellow ctermbg=NONE cterm=undercurl

    " Highlight "generalizables" (declarations in `variable` blocks) like constants:
    highlight link CornelisGeneralizable Constant
endfunction
