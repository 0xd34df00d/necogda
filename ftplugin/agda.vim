call nvimhs#start(expand('<sfile>:p:h:h'), 'necogda', [])
call InitializePlugin()
call LoadNecogda()

set completefunc=NecogdaComplete
set completeopt=menu,menuone,noinsert

inoremap <buffer><silent> ` `<C-X><C-U>

nnoremap <buffer><silent> <LocalLeader>l :w<CR>:call NecogdaLoadFile()<CR>
