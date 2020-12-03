type range = int * int

type req = Req of range * string * string

let req_satisfied (Req ((i, j), c, pwd)) =
  let c' = String.get c 0 in
  let count = List.length (String.split_on_char c' pwd) - 1 in
  i <= count && count <= j

let string_of_req (Req ((i, j), c, pwd)) =
  string_of_int i ^ " - " ^ string_of_int j ^ c ^ ": " ^ pwd

let print_req r = string_of_req r |> print_endline
