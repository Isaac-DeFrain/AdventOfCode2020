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

let nums = ref ([] : int list)

let rec parse_file filename =
   let open Core.In_channel in
   let inx = create filename in
   let ls = input_lines inx in
   nums := process_nums ls ;
   close inx

and process_nums = List.map int_of_string

(* exclusive list slice *)
let rec slice m n l =
   if m < 0 || n <= m then []
   else match l with
   | [] -> []
   | hd::tl ->
      if m = 0 then hd :: slice 0 (n - 1) tl
      else slice (m - 1) (n - 1) tl

let isValid pos num =
   let open List in
   pos < 25  ||
   let prev25 = slice (pos - 25) pos !nums in
   let sums =
      map (fun x ->
         filter ((<>) x) prev25
         |> map ((+) x)) prev25
      |> concat
   in
   mem num sums

let rec check_pos n =
   let open List in
   let len = length !nums in
   if len <= n then -1
   else if not (isValid n (nth !nums n)) then nth !nums n
   else check_pos (n + 1)

let main () =
   Printf.printf "~~~ Puzzle 9 ~~~\n" ;
   parse_file "test/puzzle9.input" ;
   Printf.printf "Pt1 solution: %d\n" (check_pos 0) ;
   ()
