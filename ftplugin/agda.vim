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
