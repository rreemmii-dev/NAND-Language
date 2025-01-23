type ppos = Lexing.position * Lexing.position

type program = gdef list

and var_value =
| Empty
| Bool of bool
| Int of int
| Tuple of var_value array

and left_value =
| VarLV of string * ppos
| TupleLV of left_value array * ppos
| TupleNewLV of string * int * ppos
| TupleItemLV of string * expr * ppos

and gdef =
| Function of string * string array * stmt * ppos

and stmt =
| Set of left_value * expr * ppos
| For of string * expr * expr * stmt * ppos
| Return of expr * ppos
| Bloc of stmt list * ppos
| Debug of string * expr * ppos

and expr =
| BCst of bool * ppos
| ICst of int * ppos
| TupleExpr of expr array * ppos
| TupleItem of string * expr * ppos
| Var of string * ppos
| Call of string * expr array * ppos
| VarValue of var_value * ppos
| Op of op * expr * expr * ppos
and op = Plus | Minus | Times | Div | Mod

let pos ((s, e) : ppos) =
    [
        ("start_line", `Int s.pos_lnum);
        ("start_char", `Int (s.pos_cnum - s.pos_bol));
        ("end_line", `Int e.pos_lnum);
        ("end_char", `Int (e.pos_cnum - e.pos_bol));
    ]
