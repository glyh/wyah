{
  open Menhir_parser

  exception LexError of string

  let[@inline] failwith msg = raise (LexError msg)

  let[@inline] illegal c =
    failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let WS = [ ' ' '\t' ]
let newline = "\r\n" | '\r' | '\n'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let int_lit = '-'? ('0' | ['1'-'9']['0'-'9']*)
let char_lit = "'" (_ # ['\'' '\\']) "'" | "'\\" ['\'' '\\'] "'"

rule next_token = parse
  | eof { EOF }
  | "\\" { FSLASH }
  | "let" { LET }
  | "=" { ASSIGN }
  | "in" { IN }
  (*| "." { DOT }*)
  (*| ":" { COLON }*)

  (*| "Int" { T_INT }*)
  (*| "Bool" { T_BOOL }*)
  (*| "Char" { T_CHAR }*)
  (*| "IO" { T_IO }*)
  | "->" { RARROW }

  | "()" { UNIT }
  | "True" { TRUE }
  | "False" { FALSE }
  | int_lit { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | char_lit { 
    let token = (Lexing.lexeme lexbuf) in
    let char_index = (String.length token) - 2 in
    CHAR (String.get token char_index)
  }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | newline { Lexing.new_line lexbuf; next_token lexbuf }
  | WS* { next_token lexbuf }
  | "{-" { block_comment lexbuf }
  | "--" { single_line_comment lexbuf }

  (* no match? raise exception *)
  | _ as c { illegal c }

(* allow nested comments, like OCaml *)
and block_comment = parse
  | newline { Lexing.new_line lexbuf; block_comment lexbuf }
  | "-}"
    {
      next_token lexbuf
    }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { block_comment lexbuf }

and single_line_comment = parse
  | newline 
    { Lexing.new_line lexbuf; next_token lexbuf }
  | eof 
    { EOF }
  | _ 
    { single_line_comment lexbuf }
