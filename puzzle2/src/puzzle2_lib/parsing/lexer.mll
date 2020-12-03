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
let range = num '-' num
let char = ['a'-'z'] | ['A'-'Z']
let pwd = char+
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

(* lexing rules *)
rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; NL }
  | range   { RANGE (String.split_on_char '-' (Lexing.lexeme lexbuf)
              |> List.map int_of_string |> fun l ->
                  match l with [a;b] -> a, b | _ -> assert false) }
  | ':'     { COLON }
  | char    { CHAR (Lexing.lexeme lexbuf) }
  | pwd     { PWD (Lexing.lexeme lexbuf)}
  | eof     { EOF }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
