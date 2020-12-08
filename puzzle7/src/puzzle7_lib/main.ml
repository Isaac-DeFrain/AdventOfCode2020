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

(* Bag (quality, string) *)
type bag = Bag of string * string

let req = ref ([] : (bag * (int * bag) list) list)

let drop_while f = Core.List.drop_while ~f
let take_while f = Core.List.take_while ~f

let color (Bag (_, c)) = c

let rec drop n l =
   if n <= 0 then l
   else match l with
   | [] -> []
   | _::tl -> drop (n - 1) tl

let rec parse_file filename =
   let open Core.In_channel in
   let inx = create filename in
   let ls = input_lines inx in
   req := List.map process_bag ls ;
   close inx

and process_bag s =
   let words s = String.split_on_char ' ' s in
   let q = List.nth (words s) 0 in
   let c = List.nth (words s) 1 in
   let contents =
      drop 4 (words s) |> String.concat " "
      |> String.split_on_char ',' |> List.map String.trim |> List.map words
   in
   Bag (q, c), process_bags contents

and process_bags = function
   | [] -> []
   | ("no"::_)::_ -> []
   | (n::q::c::_)::tl -> (int_of_string n, Bag (q, c)) :: process_bags tl
   | _ -> assert false

let fst (a, _) = a
let snd (_, b) = b

let check_contains_bag b =
   let bag_contains b (b', _) = List.assoc b' !req |> List.map snd |> List.mem b in
   List.filter (bag_contains b) !req |> List.map fst |> List.sort_uniq compare

let count_contains b =
   let sort = List.sort_uniq compare in
   let rec aux acc bs =
      let bags = List.map check_contains_bag (acc @ bs |> sort) |> List.concat in
      let accbags = bags @ acc @ bs |> sort in
      if List.for_all (fun x -> List.mem x (acc @ bs)) accbags
      then acc
      else aux accbags accbags
   in
   aux [] [b] |> List.filter ((<>) b)
   |> Core.List.dedup_and_sort ~compare |> List.length

(* part 2 *)
let count_bags_within b =
   let rec count (n, b') =
      let within = List.assoc b' !req in
      let sum_count = List.fold_left (+) 0 (List.map (fun x -> count x) within) in
      n + n * sum_count
   in
   count (1, b) - 1

let main () =
   Printf.printf "~~~ Puzzle 7 ~~~\n" ;
   parse_file "test/puzzle7.input" ;
   Printf.printf "Pt1 solution: %d\n" (count_contains (Bag ("shiny", "gold"))) ;
   Printf.printf "Pt2 solution: %d\n" (count_bags_within (Bag ("shiny", "gold")))
