let trim_list = List.filter (Core.Poly.(<>) "")

let lines s =
  String.split_on_char '\n' s
  |> List.rev
  |> List.tl
  |> List.rev

let unlines =
  let rec unlines' s = function
  | [] -> s
  | hd::tl -> unlines' (s ^ hd ^ "\n") tl
  in
  unlines' ""

(* property tests:
 - forall s. lines s |> unlines = s
 - forall l. unlines l |> lines = l
 *)

let explode =
  let rec explode' acc s =
    let open String in
    let len = length s in
    if len = 0 then List.rev acc
    else explode' (get s 0 :: acc) (sub s 1 (len - 1))
  in
  explode' []

let boolify = function
| '#' -> true
| '.' -> false
| c -> raise (Failure (Char.escaped c ^ " is not a valid char; only '#' and '.' are valid"))
