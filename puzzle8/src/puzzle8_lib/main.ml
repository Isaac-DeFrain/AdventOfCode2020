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

let seen = ref []

let acc = ref 0

let ( += ) r n = r := !r + n
let ( @= ) r n = r := n :: !r

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

let goto l ls = List.assoc l ls

let rec perform (l, I (code, n)) ls =
   let len = List.length ls in
   if List.mem l !seen then !acc
   else (seen @= l ;
   match code with
   | Acc -> acc += n ; if l + 1 > len then !acc else perform (l + 1, goto (l + 1) ls) ls
   | Jmp -> if l + n > len then !acc else perform (l + n, goto (l + n) ls) ls
   | Nop -> if l + 1 > len then !acc else perform (l + 1, goto (l + 1) ls) ls)

let does_loop ls =
   let seen = ref [] in
   let rec next l =
      let len = List.length ls in
      if List.mem l !seen then true
      else if l > len then false
      else
      let I (code, n) = List.assoc l ls in
      seen @= l ;
      match code with
      | Jmp -> next (l + n)
      | _ -> next (l + 1)
   in
   next 1

let fst (a, _) = a

let nops_and_jmps ls =
   List.filter (fun (_, I (c, _)) -> c = Jmp || c = Nop) ls
   |> List.map fst

let change_line l =
   let switch = function
   | Jmp -> Nop
   | Nop -> Jmp
   | x -> x
   in
   List.map (fun ((l', I (code, n)) as x) ->
      if l = l' then (l, I (switch code, n)) else x)

let rec correct ls = function
| [] -> print_endline "They all loop..." ; []
| hd::tl ->
   if does_loop ls then correct (change_line hd !lines) tl
   else ls

let pt2 ls = perform (List.nth ls 0) ls

let reset () = acc := 0 ; seen := []

let main () =
   Printf.printf "~~~ Puzzle 8 ~~~\n" ;
   parse_file "test/puzzle8.input" ;
   Printf.printf "Pt1 solution: %d\n" (perform (List.nth !lines 0) !lines) ;
   let c = nops_and_jmps !lines |> correct !lines in
   reset () ;
   Printf.printf "Pt2 solution: %d\n" (pt2 c)
