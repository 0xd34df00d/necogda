call nvimhs#start(expand('<sfile>:p:h:h'), 'necogda', [])
sleep 200m
call InitializePlugin()
call LoadNecogda()

set omnifunc=NecogdaComplete
set completeopt=menu,menuone,noinsert

inoremap <buffer><silent> ` `<C-X><C-O>

nnoremap <buffer><silent> <LocalLeader>l :w<CR>:call NecogdaLoadFile()<CR>
