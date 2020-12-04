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
open Processing

let tree_count = ref 0

let forest = ref []

(* starting position *)
let pos = ref 0

let width = ref 0

let slopes = [1,1; 3,1; 5,1; 7,1; 1,2]

(* read frame from file and turn into forest *)
let forest_of_file filename =
  pos := 0 ;
  tree_count := 0 ;
  let open In_channel in
  let inx = create filename in
  let ls = input_lines inx in
  forest := List.map ~f:explode ls |> List.map ~f:(List.map ~f:boolify) ;
  width := List.hd ls |> Option.value_map ~default:0 ~f:String.length ;
  close inx

(* increment *)
let ( += ) r n = r := !r + n

let x slope = let x, _ = slope in x
let y slope = let _, y = slope in y

let rec down n l =
  let open Stdlib.List in
  if n <= 0 then l
  else
  try down (n - 1) (tl l) with
  | Failure _ -> []

let move slope () =
  let open Stdlib.List in
  forest := down (y slope) !forest ;
  pos += x slope ;
  let idx = !pos mod !width in
  let open Poly in
  if !forest <> [] && nth (hd !forest) idx
  then tree_count += 1 else ()

let rec loop slope () =
  move slope () ;
  match !forest with
  | [] -> !tree_count
  | _ -> loop slope ()

let reset_and_loop slope =
  forest_of_file "test/puzzle3.input" ;
  loop slope ()

let solutions () =
  let open Stdlib.List in
  forest_of_file "test/puzzle3.input" ;
  print_endline ("Pt1 solution: " ^ string_of_int (loop (3, 1) ())) ;
  let pt2 = map reset_and_loop slopes in
  print_endline ("Pt2 solution: " ^ string_of_int (fold_left ( * ) 1 pt2))
