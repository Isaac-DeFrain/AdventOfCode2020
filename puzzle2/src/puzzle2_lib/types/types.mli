type range = int * int

type req = Req of range * string * string

val req_satisfied_pt1 : req -> bool

val req_satisfied_pt2 : req -> bool

val print_req : req -> unit
