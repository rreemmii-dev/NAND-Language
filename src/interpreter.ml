open Ast
open Format
open Lexer

exception Return of var_value


let locate_pos (pos : Lexing.position) : unit =
    let l = pos.pos_lnum in
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    eprintf "Line %d, characters %d-%d:\n" l (c - 1) c

let locate_ppos (ppos : ppos) : unit =
    let p1, p2 = ppos in
    let l1 = p1.pos_lnum in
    let c1 = p1.pos_cnum - p1.pos_bol + 1 in
    let l2 = p2.pos_lnum in
    let c2 = p2.pos_cnum - p2.pos_bol + 1 in
    eprintf "From line %d character %d to line %d character %d:\n" l1 c1 l2 (c2 - 1)

let string_of_vartype (var : var_value) : string =
    match var with
    | Empty -> "Empty"
    | Bool _ -> "Bool"
    | Int _ -> "Int"
    | Tuple _ -> "Tuple"

let error (ppos : ppos) (message : string) : 'a =
    locate_ppos ppos;
    eprintf "%s\n" message;
    exit 1


let get_program (file_name : string) : program =
    if not (Filename.check_suffix file_name ".nand") then
        failwith "Your filename extension has to be .nand";

    let f = open_in file_name in
    let buf = Lexing.from_channel f in

    try
        let p = Parser.prog Lexer.token buf in
        close_in f;
        p
    with
    | Lexer.Lexing_error c ->
        locate_pos (Lexing.lexeme_start_p buf);
        eprintf "Lexing error: %c" c;
        exit 1
    | Parser.Error ->
        locate_pos (Lexing.lexeme_start_p buf);
        eprintf "Parsing error";
        exit 1


let (any_ppos : ppos) = ({
    pos_fname = "";
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0;
}, {
    pos_fname = "";
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0;
})

type funs_hashtbl = (string, string array * stmt) Hashtbl.t
type vars_hashtbl = (string, var_value) Hashtbl.t
let (functions : funs_hashtbl) = Hashtbl.create 0

let rec interpret_expr (vars : vars_hashtbl) (expr : expr) : var_value =
    match expr with
    | BCst (b, _) ->
        Bool b
    | ICst (i, _) ->
        Int i
    | TupleExpr (expr_array, _) ->
        Tuple (Array.map (interpret_expr vars) expr_array)
    | TupleItem (var, i, p) -> (
        let index = (
            match interpret_expr vars i with
            | Int i -> i
            | x -> error p (sprintf "TypeError: When using t[i], i has to be an Int, not %s" (string_of_vartype x))
        ) in
        if not (Hashtbl.mem vars var) then
            error p (sprintf "VariableNotFound: %s" var);
        match Hashtbl.find vars var with
        | Tuple (var_array) ->
            if not (index < Array.length var_array) then
                error p (sprintf "IndexError: Tuple index (%d) out of range (0 to %d - 1)" index (Array.length var_array));
            var_array.(index)
        | x -> error p (sprintf "TypeError: When using t[i], t has to be a Tuple, not %s" (string_of_vartype x))
    )
    | Var (var, p) ->
        if not (Hashtbl.mem vars var) then
            error p (sprintf "VariableNotFound: %s" var);
        Hashtbl.find vars var
    | Call (fun_name, args_exprs, p) ->
        if fun_name = "nand" then (
            if not (Array.length args_exprs = 2) then
                error p (sprintf "TypeError: %s takes %d arguments but %d were given" fun_name 2 (Array.length args_exprs));
            let e0 = interpret_expr vars args_exprs.(0) in
            let e1 = interpret_expr vars args_exprs.(1) in
            match e0, e1 with
            | Bool b0, Bool b1 -> Bool (not (b0 && b1))
            | x1, x2 -> error p (sprintf "TypeError: %s arguments have to be Bool, not %s and %s" fun_name (string_of_vartype x1) (string_of_vartype x2))
        ) else (
            if not (Hashtbl.mem functions fun_name) then
                error p (sprintf "FunctionNotFound: %s" fun_name);
            let args_names, body = Hashtbl.find functions fun_name in
            let fun_vars = Hashtbl.create 0 in
            if not (Array.length args_exprs = Array.length args_names) then
                error p (sprintf "TypeError: %s takes %d arguments but %d were given" fun_name (Array.length args_names) (Array.length args_exprs));
            for i = 0 to Array.length args_names - 1 do
                Hashtbl.replace fun_vars args_names.(i) (interpret_expr vars args_exprs.(i))
            done;
            try (
                interpret_stmt fun_vars body;
                Empty
            ) with Return v -> v
        )
    | VarValue (v, _) ->
        v
    | Op (op, e1, e2, p) ->
        let e1, e2 = interpret_expr vars e1, interpret_expr vars e2 in
        let e1, e2 = (
            match e1, e2 with
            | Int e1, Int e2 -> e1, e2
            | x1, x2 -> error p (sprintf "TypeError: When using +, -, * or /, arguments has to be Int, not %s and %s" (string_of_vartype x1) (string_of_vartype x2))
        ) in
        match op with
        | Plus ->
            Int (e1 + e2)
        | Minus ->
            Int (e1 - e2)
        | Times ->
            Int (e1 * e2)
        | Div ->
            Int (e1 / e2)
        | Mod ->
            Int (e1 mod e2)

and interpret_stmt (vars : vars_hashtbl) (stmt : stmt) : unit =
    match stmt with
    | Set (lv, expr, _) ->
        let expr_value = interpret_expr vars expr in
        let expr_value = (
            match expr_value with
            | Int 0 -> Bool false
            | Int 1 -> Bool true
            | _ -> expr_value
        ) in (
        match lv with
        | VarLV (var, _) ->
            Hashtbl.replace vars var expr_value
        | TupleLV (left_values, p) -> (
            match expr_value with
            | Tuple vars_values ->
                if not (Array.length left_values = Array.length vars_values) then
                    error p (sprintf "TypeError: %d left values and %d right values were given" (Array.length left_values) (Array.length vars_values));
                for i = 0 to Array.length left_values - 1 do
                    interpret_stmt vars (Set (left_values.(i), VarValue (vars_values.(i), any_ppos), any_ppos))
                done
            | x -> error p (sprintf "TypeError: When using t = x with t being a Tuple, x has to be a Tuple, not %s" (string_of_vartype x))
        )
        | TupleNewLV (var, len, _) ->
            Hashtbl.replace vars var (Tuple (Array.make len expr_value))
        | TupleItemLV (var, i, p) -> (
            let index = (
                match interpret_expr vars i with
                | Int index -> index
                | x -> error p (sprintf "TypeError: When using t[i], i has to be an Int, not %s" (string_of_vartype x))
            ) in
            if not (Hashtbl.mem vars var) then
                error p (sprintf "VariableNotFound: %s" var);
            match Hashtbl.find vars var with
            | Tuple (old_values_array) ->
                old_values_array.(index) <- expr_value;
                Hashtbl.replace vars var (Tuple old_values_array)
            | x -> error p (sprintf "TypeError: When using t[i], t has to be a Tuple, not %s" (string_of_vartype x))
        )
        )
    | For (var, start, stop, body, p) ->
        let start, stop = (
            match interpret_expr vars start, interpret_expr vars stop with
            | Int start, Int stop -> start, stop
            | x1, x2 -> error p (sprintf "TypeError: When using for i = start to stop, start and stop have to be Int, not %s and %s" (string_of_vartype x1) (string_of_vartype x2))
        ) in
        if start < stop then (
            for i = start to stop do
                Hashtbl.replace vars var (Int i);
                interpret_stmt vars body
            done
        ) else (
            for i = start downto stop do
                Hashtbl.replace vars var (Int i);
                interpret_stmt vars body
            done
        );
    | Return (expr, _) ->
        let expr_value = interpret_expr vars expr in
        raise (Return expr_value)
    | Bloc (stmt_list, _) ->
        List.iter (interpret_stmt vars) stmt_list
    | Debug (param, expr, p) ->
        if param = "print" then (
            let rec aux var =
                match var with
                | Empty -> printf "Empty  "
                | Bool b -> printf "Bool %b  " b
                | Int i -> printf "Int %d  " i
                | Tuple values -> printf "Array {  "; Array.iter aux values; printf "  }  "
            in aux (interpret_expr vars expr);
            printf "\n"
        ) else if List.mem param ["int64"; "int32"; "int16"; "int8"] then (
            let nb_bits =
                if param = "int64" then 64
                else if param = "int32" then 32
                else if param = "int16" then 16
                else if param = "int8" then 8
                else -1
            in
            match interpret_expr vars expr with
            | Tuple (values_array) ->
                let n = ref 0 in
                for i = nb_bits - 1 downto 0 do
                    match values_array.(i) with
                    | Bool x -> n := !n * 2 + (if x then 1 else 0)
                    | x -> error p (sprintf "TypeError: When using debug %s t, t has to be a Tuple of Bool, not a Tuple of %s" param (string_of_vartype x))
                done;
                printf "Int %d\n" !n
            | x -> error p (sprintf "TypeError: When using debug %s t, t has to be a Tuple, not %s" param (string_of_vartype x))
        );
        flush_all ()

let interpret_gdef (gdef : gdef) : unit =
    match gdef with
    | Function (name, args_names, body, _) ->
        Hashtbl.replace functions name (args_names, body)

let interpret_program (program : program) =
    List.iter interpret_gdef program;

    let vars = Hashtbl.create 0 in
    ignore (interpret_expr vars (Call ("main", [||], any_ppos)))

let () =
    if not (Array.length Sys.argv > 1) then (
        eprintf "Use: ./interpreter.exe <filename.nand>\n";
        exit 1
    );
    let file_name = Sys.argv.(1) in
    let program = get_program file_name in
    interpret_program program
