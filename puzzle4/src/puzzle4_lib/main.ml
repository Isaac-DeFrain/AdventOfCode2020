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

let rec valid_pt2 p =
  valid_pt1 p && valid_byr p && valid_iyr p && valid_eyr p
  && valid_hgt p && valid_hcl p && valid_ecl p && valid_pid p

and valid_byr p =
  let open Stdlib in
  let byr = try List.assoc "byr" p with Not_found -> "" in
  let byr_int = try int_of_string byr with Failure _ -> 0 in
  String.length byr = 4 && 1920 <= byr_int && byr_int <= 2002

and valid_iyr p =
  let open Stdlib in
  let iyr = try List.assoc "iyr" p with Not_found -> "" in
  let iyr_int = try int_of_string iyr with Failure _ -> 0 in
  String.length iyr = 4 && 2010 <= iyr_int && iyr_int <= 2020

and valid_eyr p =
  let open Stdlib in
  let eyr = try List.assoc "eyr" p with Not_found -> "" in
  let eyr_int = try int_of_string eyr with Failure _ -> 0 in
  String.length eyr = 4 && 2020 <= eyr_int && eyr_int <= 2030

and valid_hgt p =
  let open Stdlib in
  let open String in
  let hgt = try List.assoc "hgt" p with Not_found -> "  " in
  let len = length hgt in
  let hgt_int = try int_of_string (sub hgt 0 (len - 2)) with Failure _ -> 0 in
  let units = List.map Char.escaped [get hgt (len - 2); get hgt (len - 1)] |> concat "" in
  let cm_cond = units = "cm" && 150 <= hgt_int && hgt_int <= 193 in
  let in_cond = units = "in" && 59 <= hgt_int && hgt_int <= 76 in
  cm_cond || in_cond

and valid_hcl p =
  let open Stdlib in
  let hcl = try List.assoc "hcl" p with Not_found -> "" in
  let hcl' = try String.sub hcl 1 6 with Invalid_argument _ -> "xxxxxx" in
  let hcl_list = explode hcl' in
  let valid_chars = "0123456789abcdef" in
  String.get hcl 0 = '#' && String.length hcl' = 6
  && List.for_all (String.contains valid_chars) hcl_list

and valid_ecl p =
  let open Stdlib in
  let ecl = try List.assoc "ecl" p with Not_found -> "" in
  let valid_ecl = ["amb"; "blu"; "brn"; "grn"; "gry"; "hzl"; "oth"] in
  List.exists (String.equal ecl) valid_ecl

and valid_pid p =
  let open Stdlib in
  let pid = try List.assoc "pid" p with Not_found -> "" in
  let pid_list = explode pid in
  let digit = String.contains "0123456789" in
  String.length pid = 9 && List.for_all digit pid_list

and explode =
  let rec explode' acc s =
    let open Stdlib.String in
    let len = length s in
    if len = 0 then List.rev acc
    else explode' (get s 0 :: acc) (sub s 1 (len - 1))
  in
  explode' []

let count_valid part ps = List.filter ~f:part ps |> List.length

let main () =
  parse_file "test/puzzle4.input" ;
  let ps = !passports in
  let pt1 = count_valid valid_pt1 ps |> string_of_int in
  print_endline ("Pt1 solution: " ^ pt1) ;
  let pt2 = count_valid valid_pt2 ps |> string_of_int in
  print_endline ("Pt2 solution: " ^ pt2)
