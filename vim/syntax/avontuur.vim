" Vim syntax file
" Language: Avontuur
" Maintainer: Matthijs Groen
" Latest Revision: 17 April 2019

if exists("b:current_syntax")
  finish
endif

syn keyword advTodo contained TODO FIXME XXX NOTE TEDOEN NOTITIE AANPASSEN
syn match advComment /^'.*$/ contains=advTodo
syn match advPredicate /\d\+[=><!+-r]/ contained display nextgroup=advValue
syn match advValue /\d\+/ contained display
syn match advInterpolateState /[#]\d\{2}/ contained display
syn match advInterpolateName /[$]n/ contained display
syn match advTooLong /[^"]\+/ contained display
syn match advLength /[^"]\{80}/ contained display nextgroup=advTooLong
syn region advString start='"' end='"' contains=advInterpolateName,advInterpolateState,advLength
syn region advState start=/"\(\d\|&\)\@=/ end='"' contains=advPredicate
syn region advMarkup start=/"[*]\@=/ end='"' contains=advCol1,advCol2,advCol3,advCol4,advCol5,advCol6,advCol7,advCol8,advCol9,advCol10,advCol11,advCol12,advCol13,advCol14,advCol15
syn match advTrailing /,$/ display

syn match advCol1 contained /[*]c1/
syn match advCol2 contained /[*]c2/
syn match advCol3 contained /[*]c3/
syn match advCol4 contained /[*]c4/
syn match advCol5 contained /[*]c5/
syn match advCol6 contained /[*]c6/
syn match advCol7 contained /[*]c7/
syn match advCol8 contained /[*]c8/
syn match advCol9 contained /[*]c9/
syn match advCol10 contained /[*]c10/
syn match advCol11 contained /[*]c11/
syn match advCol12 contained /[*]c12/
syn match advCol13 contained /[*]c13/
syn match advCol14 contained /[*]c14/
syn match advCol15 contained /[*]c15/

let b:current_syntax = "avontuur"

hi def link advTooLong            Error
hi def link advTrailing           Error
hi def link advTodo               Todo
hi def link advComment            Comment
hi def link advState              PreCondit
hi def link advMarkup             Keyword

hi def link advString             String
hi def link advLength             String

hi def link advValue              Constant
hi def link advPredicate          Type
hi def link advInterpolateState   Underlined
hi def link advInterpolateName    Underlined

hi advCol1 ctermfg=18
hi advCol2 ctermfg=28
hi advCol3 ctermfg=51
hi advCol4 ctermfg=196
hi advCol5 ctermfg=90
hi advCol6 ctermfg=100
hi advCol7 ctermfg=102
hi advCol8 ctermfg=16
hi advCol9 ctermfg=63
hi advCol10 ctermfg=46
hi advCol11 ctermfg=87
hi advCol12 ctermfg=203
hi advCol13 ctermfg=201
hi advCol14 ctermfg=226
hi advCol15 ctermfg=231
