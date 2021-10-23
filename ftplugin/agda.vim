call nvimhs#start(expand('<sfile>:p:h:h'), 'necogda', [])
call LoadNecogda()

set completefunc=NecogdaComplete
set completeopt=menu,menuone,noinsert

inoremap <buffer><silent> ` `<C-X><C-U>
