type range = int * int

type req = Req of range * string * string

val req_satisfied : req -> bool

val print_req : req -> unit
