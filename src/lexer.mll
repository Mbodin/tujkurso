{ (** Module Lexer. **)

  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos ; pos_lnum = 1 + pos.pos_lnum }

  let current_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    (if pos.pos_fname <> "" then "file `" ^ pos.pos_fname ^ "', " else "")
    ^ "line " ^ string_of_int pos.pos_lnum ^ ", "
    ^ "character " ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)
}

let space = ' ' | '\t'
let newline = '\n' | '\r' | "\r\n"

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let ident = letter (letter | digit | '_')*

rule read = parse

  | newline             { next_line lexbuf ; SPACE }
  | space+              { SPACE }
  | eof                 { EOF }

  | '\\' (ident as id)  { COMMAND id }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }

  | "\\\\"              { STRING "\\" }
  | "\\{"               { STRING "{" }
  | "\\}"               { STRING "}" }

  | [^ ' ' '\t' '\n' '\r' '\\' '{' '}' ]*   { STRING c }

