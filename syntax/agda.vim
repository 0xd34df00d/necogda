" Inspired by https://github.com/derekelkins/agda-vim/blob/master/syntax/agda.vim
syn match   agdaCharCode     contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   agdaCharCode     contained "\v\\(NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL)"
syn region  agdaString       start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=agdaCharCode
syn match   agdaHole         "\v(^|\s|[.(){};])@<=(\?)($|\s|[.(){};])@="
syn region  agdaX            matchgroup=agdaHole start="{!" end="!}" contains=ALL
syn match   agdaLineComment  "\v(^|\s|[.(){};])@<=--.*$" contains=@agdaInComment
syn region  agdaBlockComment start="{-"  end="-}" contains=agdaBlockComment,@agdaInComment
syn region  agdaPragma       start="{-#" end="#-}"
syn cluster agdaInComment    contains=agdaTODO,agdaFIXME,agdaXXX
syn keyword agdaTODO         contained TODO
syn keyword agdaFIXME        contained FIXME
syn keyword agdaXXX          contained XXX

hi def link agdaHoleVirtualText  CocWarningVirtualText
hi def      agdaTODO             cterm=bold,underline ctermfg=2 " green
hi def      agdaFIXME            cterm=bold,underline ctermfg=3 " yellow
hi def      agdaXXX              cterm=bold,underline ctermfg=1 " red


hi def link agda_atom_keyword Keyword
hi def link agda_atom_comment Comment
hi def link agda_atom_background Comment
hi def link agda_atom_markup Comment
hi def link agda_atom_string String
hi def link agda_atom_number Number
hi def link agda_atom_symbol Special
hi def link agda_atom_primitivetype Type
hi def link agda_atom_argument Identifier
hi def link agda_atom_bound Identifier
hi def link agda_atom_generalizable Identifier
hi def link agda_atom_inductiveconstructor Normal
hi def link agda_atom_coinductiveconstructor Normal
hi def link agda_atom_datatype Type
hi def link agda_atom_field Normal
hi def link agda_atom_function Normal
hi def link agda_atom_module Structure
hi def link agda_atom_postulate Normal
hi def link agda_atom_pragma PreProc
hi def link agda_atom_primitive Normal
hi def link agda_atom_macro Macro
hi def link agda_atom_record Type
hi def link agda_atom_dotted Normal
hi def link agda_atom_dottedpattern Normal
hi def link agda_atom_operator Operator
hi def link agda_atom_error Error
hi def link agda_atom_unsolvedmeta Underlined
hi def link agda_atom_unsolvedconstraint Underlined
hi def link agda_atom_terminationproblem Underlined
hi def link agda_atom_deadcode Underlined
hi def link agda_atom_coverageproblem Underlined
hi def link agda_atom_positivityproblem Underlined
hi def link agda_atom_incompletepattern Underlined
hi def link agda_atom_catchallclause Underlined
hi def link agda_atom_confluenceproblem Underlined
hi def link agda_atom_missingdefinition Underlined
hi def link agda_atom_typechecks Normal
hi def link agda_atom_hole WarningMsg
