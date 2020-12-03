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

let num_correct data = List.filter ~f:req_satisfied data |> List.length

let print_elems d data =
  if d then List.iter ~f:print_req data

(* handler function *)
let handler f d n () =
  let open In_channel in
  let handle d n data =
    match d, n with
    | true, true ->
        print_endline "-----original-----" ;
        print_elems d data ;
        print_endline "-----two-nums-----" ;
        Printf.printf "%d\n" (num_correct data)
    | true, _ ->
        print_elems d data
    | false, true -> Printf.printf "%d\n" (num_correct data)
    | _ -> ()
  in
  match f with
  | None -> print_endline "Nothing to see here..."
  | Some filename ->
      let inx = create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      let data = parse_with_error lexbuf in
      handle d n data ;
      close inx

(* cli *)
let () =
  Command.basic_spec
    ~summary:"Parse and correct passwords"
    Command.Spec.(
      empty
      +> flag "-f" (optional string) ~doc:" Read given file"
      +> flag "-d" no_arg ~doc:" Display the correct passwords"
      +> flag "-n" no_arg ~doc:" How many passwords are correct?")
    handler
  |> Command.run

