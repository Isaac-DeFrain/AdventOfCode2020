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
open Types

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try reqs read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      []
  | Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

let num_correct_pt1 data = List.filter ~f:req_satisfied_pt1 data |> List.length

let num_correct_pt2 data = List.filter ~f:req_satisfied_pt2 data |> List.length

let print_elems d data =
  if d then List.iter ~f:print_req data

(* handler function *)
let handler f d pt1 pt2 () =
  let open In_channel in
  let display_num_pt1 data = Printf.printf "%d\n" (num_correct_pt1 data) in
  let display_num_pt2 data = Printf.printf "%d\n" (num_correct_pt2 data) in
  let handle d pt1 pt2 data =
    match d, pt1, pt2 with
    | true, true, true ->
        print_endline "-----original-----" ;
        print_elems d data ;
        print_endline "-----pt1-valid-----" ;
        display_num_pt1 data;
        print_endline "-----pt2-valid-----" ;
        display_num_pt2 data
    | true, true, _ ->
        print_endline "-----original-----" ;
        print_elems d data ;
        print_endline "-----pt1-valid-----" ;
        display_num_pt1 data
    | true, false, true ->
        print_endline "-----original-----" ;
        print_elems d data ;
        print_endline "-----pt2-valid-----" ;
        display_num_pt2 data
    | true, false, _ -> print_elems d data
    | false, true, true ->
        print_endline "-----pt1-valid-----" ;
        display_num_pt1 data;
        print_endline "-----pt2-valid-----" ;
        display_num_pt2 data
    | false, true, _ -> display_num_pt1 data
    | false, false, true -> display_num_pt2 data
    | _ -> ()
  in
  match f with
  | None -> print_endline "Nothing to see here..."
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      let data = parse_with_error lexbuf in
      handle d pt1 pt2 data ;
      close inx

(* cli *)
let () =
  Command.basic_spec
    ~summary:"Parse and correct passwords"
    Command.Spec.(
      empty
      +> flag "-f" (optional string) ~doc:" Read given file"
      +> flag "-d" no_arg ~doc:" Display the correct passwords"
      +> flag "-pt1" no_arg ~doc:" How many passwords are correct according to pt1 rules?"
      +> flag "-pt2" no_arg ~doc:" How many passwords are correct according to pt2 rules?")
    handler
  |> Command.run

