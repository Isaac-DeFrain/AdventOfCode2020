/* Parser */

/* tokens */
%token <int * int> RANGE
%token <string> PWD
%token <string> CHAR
%token COLON NL
%token EOF

%start <Types.req list> reqs
%type  <Types.req list> req_list
%type  <Types.req> req
%%

reqs:
  | req_list EOF                       { $1 } ;

req:
  | RANGE CHAR COLON PWD  { Types.Req ($1, $2, $4) } ;

req_list:
  rs = separated_list(NL, req)          { rs } ;
