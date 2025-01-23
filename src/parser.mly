%{
    open Ast
%}

%token <bool> BCST
%token <int> ICST
%token <string> ID
%token FUN FOR TO RETURN DEBUG
%token EOF
%token LP RP LB RB LSB RSB
%token EQUAL
%token SEMICOLON COMMA
%token PLUS MINUS TIMES DIV MOD

%start prog

%type <Ast.program> prog

%%

prog:
| p = list(gdef) EOF  { p }
;

gdef:
| FUN id = ID LP args_list = separated_list(COMMA, ID) RP body = stmt  { Function(id, Array.of_list args_list, body, $loc) }
;

stmt:
| lv = left_value EQUAL e = expr SEMICOLON  { Set(lv, e, $loc) }
| FOR v = ID EQUAL start = expr TO stop = expr body = stmt  { For(v, start, stop, body, $loc) }
| RETURN e = expr SEMICOLON  { Return(e, $loc) }
| LB bloc = list(stmt) RB  { Bloc(bloc, $loc) }
| DEBUG param = ID e = expr SEMICOLON  { Debug(param, e, $loc) }
;

left_value:
| v = ID  { VarLV(v, $loc) }
| lv0 = left_value COMMA lvs = separated_list(COMMA, left_value)  { TupleLV(Array.of_list (lv0 :: lvs), $loc) }
| v = ID LB i = ICST RB  { TupleNewLV(v, i, $loc) }
| v = ID LSB i = expr RSB  { TupleItemLV(v, i, $loc) }
;

expr:
| c = BCST  { BCst(c, $loc) }
| c = ICST  { ICst(c, $loc) }
| LP e0 = expr COMMA values = separated_list(COMMA, expr) RP  { TupleExpr(Array.of_list (e0 :: values), $loc) }
| v = ID LSB i = expr RSB  { TupleItem(v, i, $loc) }
| v = ID  { Var(v, $loc) }
| fct = ID LP args_list = separated_list(COMMA, expr) RP  { Call(fct, Array.of_list args_list, $loc) }
| e1 = expr o = op e2 = expr  { Op(o, e1, e2, $loc) }
| LP e = expr RP  { e }
;

%inline op:
| PLUS  { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV   { Div }
| MOD   { Mod }
;
