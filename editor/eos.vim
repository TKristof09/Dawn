
" Vim syntax file
" Language: Eos

" Usage Instructions
" Put this file in .config/nvim/syntax/eos.vim
" and add the following autocommand
" vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"},
" {
"     pattern = {"*.eos"},
"     callback = function()
"         vim.bo.filetype = "eos"
"     end
" }):

if exists("b:current_syntax")
  finish
endif

syntax keyword todos TODO XXX FIXME NOTE

" Language keywords
syntax keyword eosKeywords if else while let fun
" Type names the compiler recognizes
syntax keyword eosTypeNames i64 i32 i16 i8 u64 u32 u16 u8 f64 f32 bool str

" Comments
syntax region eosCommentLine start="//" end="$" contains=eosTodos

" String literals
syntax region eosString start=/\v"/ skip=/\v\\./ end=/\v"/ contains=eosEscapes

" Char literals
syntax region eosChar start=/\v'/ skip=/\v\\./ end=/\v'/ contains=eosEscapes

" Escape literals \n, \r, ....
syntax match eosEscapes display contained "\\[nr\"']"

" Number literals
syntax region eosNumber start=/\s\d/ skip=/\d/ end=/\s/

" Set highlights
highlight default link eosTodos Todo
highlight default link eosKeywords Keyword
highlight default link eosCommentLine Comment
highlight default link eosString String
highlight default link eosNumber Number
highlight default link eosTypeNames Type
highlight default link eosChar Character
highlight default link eosEscapes SpecialChar

let b:current_syntax = "eos"
