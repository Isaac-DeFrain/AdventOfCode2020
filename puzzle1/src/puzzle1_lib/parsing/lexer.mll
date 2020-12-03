(* Lexer *)

{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

(* convenient REs *)
let num = ['0'-'9']+
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

(* lexing rules *)
rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; NL }
  | num     { NUM (int_of_string (match Lexing.lexeme lexbuf with "" -> "0" | s -> s)) }
  | eof     { EOF }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
