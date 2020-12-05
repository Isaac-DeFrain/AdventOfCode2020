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

let convert_row = function
| 'F' -> 0
| 'B' -> 1
| _ -> assert false

let convert_col = function
| 'L' -> 0
| 'R' -> 1
| _ -> assert false

let row s =
   let open String in
   List.map convert_row
      [get s 0; get s 1; get s 2; get s 3; get s 4; get s 5; get s 6]
   |> List.fold_left (fun acc x -> 2 * acc + x) 0

let col s =
   let open String in
   List.map convert_col
      [get s 7; get s 8; get s 9]
   |> List.fold_left (fun acc x -> 2 * acc + x) 0

let seat_id s =
   let r = row s in
   let c = col s in
   r * 8 + c

let max_list = List.fold_left (fun a b -> max a b) 0

let seats = ref []

let (--) i j = 
   let rec aux n acc =
     if n < i then acc else aux (n-1) (n :: acc)
   in aux j []

let rem_seats = ref (0--1023)

let parse_file filename =
   let open Core.In_channel in
   let hdl = create filename in
   let ls = input_lines hdl |> List.map seat_id in
   seats := ls ;
   close hdl

let main () =
   Printf.printf "Puzzle 5\n" ;
   parse_file "test/puzzle5.input" ;
   Printf.printf "Pt1 soultion: %d\n" (max_list !seats) ;
   List.iter (fun x -> rem_seats := List.filter ((<>) x) !rem_seats) !seats ;
   rem_seats := List.filter (fun x -> 100 <= x && x <= 900) !rem_seats ;
   Printf.printf "Pt2 soultion: %s\n"
      begin match !rem_seats with
      | [x] -> string_of_int x
      | _ -> "not unique!!!"
      end
