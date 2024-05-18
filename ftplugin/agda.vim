if (get(g:, 'necogda_started', 0) == 0)
  call nvimhs#start(expand('<sfile>:p:h:h'), 'necogda', [])
  while !exists('*InitializePlugin')
    sleep 100m
  endwhile
  sleep 200m
  call InitializePlugin()

  let g:necogda_started = 1
endif

call LoadNecogda()

set omnifunc=NecogdaComplete
set completeopt=menu,menuone,noinsert,noselect

inoremap <buffer><silent> ` `<C-X><C-O>

nnoremap <buffer><silent> <LocalLeader>l :w<CR>:call NecogdaLoadFile()<CR>
nnoremap <buffer><silent> <LocalLeader>r :call NecogdaRefine()<CR>
nnoremap <buffer><silent> <LocalLeader>c :call NecogdaMakeCase()<CR>
nnoremap <buffer><silent> <LocalLeader>a :call NecogdaAutoOne()<CR>

nnoremap <buffer><silent> <LocalLeader>gp :call NecogdaGoalPrev()<CR>
nnoremap <buffer><silent> <LocalLeader>gn :call NecogdaGoalNext()<CR>

nnoremap <buffer><silent> <LocalLeader>? :call NecogdaHowToEnter()<CR>
