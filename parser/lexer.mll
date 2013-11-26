{
  (* The header *)
  open Lexing
}

rule lexer = parse
  | [' ' '\t'] {lexer lexbuf}
  | "//"_*'\n' {let () = new_line lexbuf in lexer lexbuf}
  | '\n' {let () = new_line lexbuf in lexer lexbuf}
  | ';'  {Parser.TSEMICOLON}
  | ";;"  {Parser.TSEMISEMI}
  | '('  {Parser.TOP}
  | ')'  {Parser.TCP}
  | '{'  {Parser.TOB}
  | '}'  {Parser.TCB}
  | '!'  {Parser.TXCL}
  | '?'  {Parser.TQ}
  | "immediate" {Parser.Timm}
  | "await" {Parser.TAwait}
  | "pause" {Parser.TPause}
  | "input" {Parser.TIn}
  | "output" {Parser.TOut}
  | "signal" {Parser.TSignal}
  | "channel" {Parser.TChannel}
  | "&" {Parser.And}
  | ":" {Parser.TColon}
  | "par" {Parser.TSplit}
  | "|" {Parser.Or}
  | "trap" {Parser.TTrap}
  | "loop" {Parser.TWhile}
  | "true" {Parser.TTrue}
  | "false" {Parser.TFalse}
  | "><" {Parser.TASYNC}
  | "if" {Parser.TPresent}
  | "else" {Parser.TElse}
  | "abort" {Parser.TAbort}
  | "send" {Parser.TSend}
  | "receive" {Parser.TReceive}
  | "suspend" {Parser.TSuspend}
  | "exit" {Parser.TExit}
  | "emit" {Parser.TEmit}
  | "int" {Parser.TInt32s}
  | "short" {Parser.TInt16s}
  | "do" {Parser.TDo}
  | "byte" {Parser.TInt8s}
  | "extern" {Parser.TExtern}
  | "bit" {Parser.TInt1s}
  | '+' {Parser.TPlus}
  | '-' {Parser.TMinus}
  | '*' {Parser.TTimes}
  | "switch" {Parser.TCase}
  | "weak" {Parser.TWeak}
  | '_' {Parser.TOtherwise}
  | '/' {Parser.TDiv}
  | '%' {Parser.TMod}
  | '^' {Parser.TXor}
  | "^^" {Parser.TPow}
  | "==" {Parser.TEqualEqual}
  | "=" {Parser.TEqual}
  | "<=" {Parser.TLessEqual}
  | ">=" {Parser.TGreaterEqual}
  | ','  {Parser.TComma}
  | '<'  {Parser.TLess}
  | '>'  {Parser.TGreater}
  | ">>"  {Parser.TRShift}
  | "<<"  {Parser.TLShift}
  | "!="  {Parser.TNotEqual}
  | "op+" {Parser.TOpPlus}
  | "op*" {Parser.TOpTimes}
  | "for" {Parser.TFor}
  | "#" {Parser.THash}
  | "begin" {Parser.TBegin}
  | "end" {Parser.TEnd}
  | ['0'-'9']+ {Parser.TInt (lexeme lexbuf)} (* an integer *)
  | ['0'-'9']+'.'['0'-'9']+ {Parser.TFloat (lexeme lexbuf)} (* a floating number *)
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* {Parser.TSymbol (lexeme lexbuf)} (* any identifier a letter followed by anything, except a '$' sign*)
  | "/*" {comment 1 lexbuf} (* start of a comment *)
  | _  {lexer lexbuf} (* leave anything else *)
  | eof {Parser.TEof}
and comment depth = parse
  | "/*" {comment (depth + 1) lexbuf}
  | "*/" {if depth = 1 then lexer lexbuf else comment (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment depth lexbuf}
  | _ {comment depth lexbuf} 

{
(* The tail for now do nothing*)
}
