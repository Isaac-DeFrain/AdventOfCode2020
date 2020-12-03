(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 Isaac-DeFrain <isaacdefrain@gmail.com>             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Core
open Lexing
open Parsing.Lexer
open Parsing.Parser

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try nums read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      []
  | Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

let find_elements data =
  List.map ~f:(fun x ->
    List.map ~f:(fun y -> y, x, x + y) data) data
  |> List.filter ~f:(fun l -> List.length l > 0)

let rec print_ans strs = function
| [] -> if Poly.(<>) strs "" then print_endline strs
| (i,j,k)::tl ->
    if k <> 2020 then print_ans strs tl
    else
    let i' = string_of_int i in
    let j' = string_of_int j in
    let sum = string_of_int (i + j) in
    let prod = string_of_int (i * j) in
    print_ans (String.concat ~sep:" "
      [i'; "+"; j'; "="; sum; "&&"; i'; "*"; j'; "="; prod]) tl

let print_ans' = print_ans ""

let print_elems d data =
  if d then List.iter ~f:(Printf.printf "%d\n") data

let print_product res data =
  if res then find_elements data |> List.iter ~f:print_ans'

(* handler function *)
let handler f d res () =
  let open In_channel in
  let handle d res data =
    match d, res with
    | true, true ->
        print_elems d data ;
        print_endline "--------------------" ;
        print_product res data
    | true, _ -> print_elems d data
    | false, true -> print_product res data
    | _ -> ()
  in
  match f with
  | None -> print_endline "Nothing to see here..."
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      let data = parse_with_error lexbuf in
      handle d res data ;
      close inx

(* cli *)
let () =
  Command.basic_spec
    ~summary:"Parse and display num list"
    Command.Spec.(
      empty
      +> flag "-f" (optional string) ~doc:" Read given file"
      +> flag "-d" no_arg ~doc:" Display the num list"
      +> flag "-2020" no_arg ~doc:" Display nums that add to 2020")
    handler
  |> Command.run
