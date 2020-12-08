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

type code = Acc | Jmp | Nop

let code_of_string = function
| "acc" -> Acc
| "jmp" -> Jmp
| "nop" -> Nop
| s -> raise (Invalid_argument s)

type instruction = I of code * int

let lines = ref ([] : (int * instruction) list)

let acc = ref 0

let ( += ) r n = r := !r + n

let seen = ref ([] : int list)

let parse_op s =
   match String.split_on_char ' ' s with
   | [c; n] -> I (code_of_string c, int_of_string n)
   | _ -> raise (Failure s)

let parse_file filename =
   let open Core.In_channel in
   let inx = create filename in
   let ls = input_lines inx in
   let line_nums =
      let num = ref 0 in
      List.map (fun l -> num += 1 ; (!num, l))
   in
   lines := List.map parse_op ls |> line_nums ;
   close inx

let goto l = List.assoc l !lines

let rec perform (l, I (code, n)) =
   if List.mem l !seen then !acc
   else (seen := l :: !seen ;
   match code with
   | Acc -> acc += n ; perform (l + 1, goto (l + 1))
   | Jmp -> perform (l + n, goto (l + n))
   | Nop -> perform (l + 1, goto (l + 1)))

let main () =
   Printf.printf "~~~ Puzzle 8 ~~~\n" ;
   parse_file "test/puzzle8.input" ;
   let first = List.hd !lines in
   Printf.printf "Pt1 solution: %d\n" (perform first);
   ()
