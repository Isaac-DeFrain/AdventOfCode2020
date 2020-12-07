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

let yeses_pt1 = ref ([] : int list)

let yeses_pt2 = ref ([] : int list)

let drop_while f = Core.List.drop_while ~f

let take_while f = Core.List.take_while ~f

let non_empty = Core.Poly.(<>) ""

let tail = List.tl

let dedup_and_sort = Core.List.dedup_and_sort ~compare:String.compare

(* exclusive range *)
let ( -- ) a b =
   let rec aux acc x y =
      if x >= y then List.rev acc
      else aux (x :: acc) (x + 1) y
   in
   aux [] a b

let explode s =
   let len = String.length s in
   let chars = List.map (String.get s) (0 -- len) in
   List.map Char.escaped chars

module StrSet = Set.Make(String)

type part = Pt1 | Pt2

let rec parse_file filename =
   let open Core.In_channel in
   let inx = create filename in
   let ls = input_lines inx in
   yeses_pt1 := process Pt1 ls ;
   yeses_pt2 := process Pt2 ls ;
   close inx

and process part ls =
   match part with
   | Pt1 ->
      groups ls |> List.map (List.map explode)
      |> List.map List.concat |> List.map dedup_and_sort
      |> List.map List.length
   | Pt2 ->
      let open Char in
      let all =
         StrSet.of_list (List.map Char.chr (code 'a' -- (code 'z' + 1))
         |> List.map Char.escaped)
      in
      groups ls |> List.map (List.map explode)
      |> List.map (List.map StrSet.of_list)
      |> List.map (List.fold_left StrSet.inter all)
      |> List.map StrSet.cardinal

and groups = function
   | [] -> []
   | ls ->
      let next = take_while non_empty ls in
      let rem =
         try drop_while non_empty ls |> tail with
         | Failure _ -> []
      in
      next :: groups rem

let solution = List.fold_left ( + ) 0

let main () =
   Printf.printf "~~~ Puzzle 6 ~~~\n" ;
   parse_file "test/puzzle6.input" ;
   Printf.printf "Pt1 solution: %d\n" (solution !yeses_pt1) ;
   Printf.printf "Pt2 solution: %d\n" (solution !yeses_pt2)
