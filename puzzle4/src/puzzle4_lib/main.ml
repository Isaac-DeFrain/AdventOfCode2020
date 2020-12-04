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

let passports = ref []

let all_fields =
   let open Stdlib in
   List.sort_uniq String.compare
      ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "cid"; "hgt"]

let all_except_cid =
  let open Stdlib in
  List.sort_uniq String.compare
     ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "hgt"]

let assocs l =
  let rec assocs' = function
  | [] -> []
  | [a; b]::rest -> (a, b) :: assocs' rest
  | _ -> raise (Failure "assocs'")
  in
  List.map ~f:(String.split ~on:':') l |> assocs'

let format_list =
  let open List in
  let rec format' acc = function
  | [] -> acc
  | l ->
      let p =
        take_while ~f:(String.(<>) "") l
        |> String.concat ~sep:" " |> String.split ~on:' '
      in
      let rest' = drop_while ~f:(String.(<>) "") l in
      let rest = try Stdlib.List.tl rest' with Failure _ -> [] in
      format' (assocs p :: acc) rest
  in
  format' []

let parse_file filename =
  let open In_channel in
  let inx = create filename in
  let ls = input_lines inx in
  passports := format_list ls ;
  close inx

let fields (l : (string * string) list) =
   let open Stdlib in
  let rec fields' acc = function
  | [] -> List.sort_uniq String.compare acc
  | (f,_)::tl -> fields' (f :: acc) tl
  in
  fields' [] l

let valid_pt1 p =
   let open Poly in
   let fp = fields p in
   fp = all_fields || fp = all_except_cid

let count_valid_pt1 ps = List.filter ~f:valid_pt1 ps |> List.length

let main () =
  parse_file "test/puzzle4.input" ;
  let pt1 = count_valid_pt1 !passports |> string_of_int in
  print_endline ("Pt1 solution: " ^ pt1)
