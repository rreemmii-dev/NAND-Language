{
    open Lexing
    open Parser

    exception Lexing_error of char

    let id_or_kwd s =
        match s with
        | "fun" -> FUN
        | "for" -> FOR
        | "to" -> TO
        | "return" -> RETURN
        | "debug" -> DEBUG
        | "True" -> BCST (true)
        | "False" -> BCST (false)
        | s -> ID s
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']
let anything_but_star = [^'*']*
let anything_but_end_comment = anything_but_star ('*' [^'/'] anything_but_star)*

rule token = parse
    | '\n'  { new_line lexbuf; token lexbuf }
    | space+  { token lexbuf }
    | "/*" anything_but_end_comment as s "*/"  { for _ = 1 to List.length (String.split_on_char '\n' s) - 1 do new_line lexbuf done; token lexbuf }
    | ident as id  { id_or_kwd id }
    | '='  { EQUAL }
    | '('  { LP }
    | ')'  { RP }
    | '{'  { LB }
    | '}'  { RB }
    | '['  { LSB }
    | ']'  { RSB }
    | ';'  { SEMICOLON }
    | ','  { COMMA }

    | '+'  { PLUS }
    | '-'  { MINUS }
    | '*'  { TIMES }
    | '/'  { DIV }
    | '%'  { MOD }

    | integer as s  { ICST (int_of_string s) }
    | eof  { EOF }
    | _ as c  { raise (Lexing_error c) }
