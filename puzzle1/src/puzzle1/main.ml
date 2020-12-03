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

let find_two_elements data =
  List.map ~f:(fun x ->
    List.map ~f:(fun y -> y, x, x + y) data) data
  |> List.filter ~f:(fun l -> List.length l > 0)

let find_three_elements data =
  List.map ~f:(fun x ->
    List.map ~f:(fun y -> y, x, x + y) data) data
  |> List.concat
  |> List.map ~f:(fun (x, y, z) ->
      List.map ~f:(fun w -> w, x, y, w + z) data)
  |> List.filter ~f:(fun l -> List.length l > 0)

let rec print_two_ans strs = function
| [] -> if Poly.(<>) strs "" then print_endline strs
| (i,j,k)::tl ->
    if k <> 2020 then print_two_ans strs tl
    else
    let i' = string_of_int i in
    let j' = string_of_int j in
    let sum = string_of_int (i + j) in
    let prod = string_of_int (i * j) in
    print_two_ans (String.concat ~sep:" "
      [i'; "+"; j'; "="; sum; "&&"; i'; "*"; j'; "="; prod]) tl

let rec print_three_ans strs = function
| [] -> if Poly.(<>) strs "" then print_endline strs
| (i,j,k,l)::tl ->
    if l <> 2020 then print_three_ans strs tl
    else
    let i' = string_of_int i in
    let j' = string_of_int j in
    let k' = string_of_int k in
    let sum = string_of_int (i + j + k) in
    let prod = string_of_int (i * j * k) in
    print_three_ans (String.concat ~sep:" "
      [i'; "+"; j'; "+"; k'; "="; sum; "&&"; i'; "*"; j'; "*"; k'; "="; prod]) tl

let print_two_ans' = print_two_ans ""

let print_three_ans' = print_three_ans ""

let print_elems d data =
  if d then List.iter ~f:(Printf.printf "%d\n") data

let print_two_product two data =
  if two then find_two_elements data |> List.iter ~f:print_two_ans'

let print_three_product three data =
  if three then find_three_elements data |> List.iter ~f:print_three_ans'

(* handler function *)
let handler f d two three () =
  let open In_channel in
  let handle d two three data =
    match d, two, three with
    | true, true, true ->
        print_endline "-----original-----" ;
        print_elems d data ;
        print_endline "-----two-nums-----" ;
        print_two_product two data ;
        print_endline "-----three-nums-----" ;
        print_three_product three data
    | true, true, _ ->
        print_endline "-----original-----" ;
        print_elems d data ;
        print_endline "-----two-nums-----" ;
        print_two_product two data
    | false, true, true ->
        print_endline "-----two-nums-----" ;
        print_two_product two data ;
        print_endline "-----three-nums-----" ;
        print_three_product three data
    | false, true, _ ->
        print_endline "-----two-nums-----" ;
        print_two_product two data
    | false, false, true -> print_three_product three data
    | _ -> ()
  in
  match f with
  | None -> print_endline "Nothing to see here..."
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      let data = parse_with_error lexbuf in
      handle d two three data ;
      close inx

(* cli *)
let () =
  Command.basic_spec
    ~summary:"Parse and display num list"
    Command.Spec.(
      empty
      +> flag "-f" (optional string) ~doc:" Read given file"
      +> flag "-d" no_arg ~doc:" Display the num list"
      +> flag "-2" no_arg ~doc:" Display two nums that add to 2020"
      +> flag "-3" no_arg ~doc:" Display three nums that add to 2020")
    handler
  |> Command.run
