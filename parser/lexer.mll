{
  (* The header *)
  open Lexing
}

rule lexer = parse
  | [' ' '\t'] {lexer lexbuf}
  | "//"_*'\n' {let () = new_line lexbuf in lexer lexbuf}
  | '\n' {let () = new_line lexbuf in lexer lexbuf}
  | ['0'-'9']+ {Parser.TInt (lexeme lexbuf)} (* an integer *)
  | ['0'-'9']+'.'['0'-'9']+ {Parser.TFloat (lexeme lexbuf)} (* a floating number *)
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* {Parser.IDENTIFIER (lexeme lexbuf)} (* any identifier a letter followed by anything, except a '$' sign*)
  | "(*" {comment 1 lexbuf} (* start of a comment *)
  | _  {lexer lexbuf} (* leave anything else *)
  | eof {Parser.TEof}
and comment depth = parse
  | "(*" {comment (depth + 1) lexbuf}
  | "*)" {if depth = 1 then lexer lexbuf else comment (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment depth lexbuf}
  | _ {comment depth lexbuf} 

{
(* The tail for now do nothing*)
}
