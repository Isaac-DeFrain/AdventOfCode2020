/* Parser */

/* tokens */
%token <int> NUM
%token NL
%token EOF

%start <int list> nums
%type  <int list> num_list
%type  <int> num
%%

nums:
  | EOF                        { [] }
  | num_list EOF               { $1 } ;

num:
  | NUM                        { $1 } ;

num_list:
  n = separated_list(NL, num)   { n } ;
