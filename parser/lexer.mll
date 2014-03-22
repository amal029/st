{
  (* The header *)
  open Lexing;;
  open Parser;;
}

rule lexer = parse
  | [' ' '\t'] {lexer lexbuf}
  | "0---T" {TOF}
  | "ADD" {ADD}
  | "ACTION" {ACTION}
  | "ANDN" {ANDN}
  | "AND" {AND}
  | "ARRAY" {ARRAY}
  | "AT" {AT}
  | "BOOL" {BOOL}
  | "BY" {BY}
  | "BYTE" {BYTE}
  | "CALCN" {CALCN}
  | "CALC" {CALC}
  | "CAL" {CAL}
  | "CD" {CD}
  | "CLK" {CLK}
  | "CONCAT" {CONCAT}
  | "CONSTANT" {CONSTANT}
  | "CTD" {CTD}
  | "CTUD" {CTUD}
  | "CTU" {CTU}
  | "CU" {CU}
  | "DATE_AND_TIME" {DATE_AND_TIME}
  | "DATE" {DATE}
  | "DINT" {DINT}
  | "DIV" {DIV}
  | "DO" {DO}
  | "DT" {DATE_AND_TIME}
  | "DWORD" {DWORD}
  | "ELSE" {ELSE}
  | "ELSIF" {ELSIF}
  | "END_ACTION" {END_ACTION}
  | "END_CASE" {END_CASE}
  | "END_FOR" {END_FOR}
  | "END_FUNCTION_BLOCK" {END_FUNCTION_BLOCK}
  | "END_FUNCTION" {END_FUNCTION}
  | "END_IF" {END_IF}
  | "END_PROGRAM" {END_PROGRAM}
  | "END_REPEAT"  {END_REPEAT}
  | "END_RESOURCE" {END_RESOURCE}
  | "END_STEP"  {END_STEP}
  | "END_STRUCT"  {END_STRUCT}
  | "END_VAR"  {END_VAR}
  | "END_WHILE"  {END_WHILE}
  | "EQ"  {EQ}
  | "EXIT"  {EXIT}
  | "EXPT"  {EXPT}
  | "FALSE"  {FALSE}
  | "F_EDGE"  {F_EDGE}
  | "FOR"  {FOR}
  | "FROM"  {FROM}
  | "F_TRIG"  {F_TRIG}
  | "FUNCTION_BLOCK"  {FUNCTION_BLOCK}
  | "FUNCTION"  {FUNCTION}
  | "GE"  {GE}
  | "GT"  {GT}
  | "IF"  {IF}
  | "INITIAL_STEP"  {INITIAL_STEP}
  | "IN" {IN}
  | "INSERT"  {INSERT}
  | "INTERVAL"  {INTERVAL}
  | "INT"  {INT}
  | "JMPCN" {JMPCN}
  | "JMPC" {JMPC}
  | "JMP" {JMP}
  | "LD" {LD}
  | "LE" {LE}
  | "LIMIT" {LIMIT}
  | "LINT" {LINT}
  | "LREAL" {LREAL}
  | "LT" {LT}
  | "LWORD" {LWORD}
  | "MAX" {MAX}
  | "MIN" {MIN}
  | "MOD" {MOD}
  | "MOVE" {MOVE}
  | "MUL" {MUL}
  | "MUX" {MUX}
  | "NE" {NE}
  | "NOT" {NOT}
  | "OF" {OF}
  | "ON" {ON}
  | "ORN" {ORN}
  | "OR" {OR}
  | "PRIORITY" {PRIORITY}
  | "PROGRAM" {PROGRAM}
  | "REAL" {REAL}
  | "R_EDGE" {R_EDGE}
  | "REPEAT" {REPEAT}
  | "RESOURCE" {RESOURCE}
  | "RETAIN" {RETAIN}
  | "RETURN" {RETURN}
  | "RS" {RS}
  | "R_TRIG" {R_TRIG}
  | "SEL" {SEL}
  | "SINGLE" {SINGLE}
  | "SINT" {SINT}
  | "SR" {SR}
  | "STEP" {STEP}
  | "ST" {ST}
  | "STRING" {STRING}
  | "STRUCT" {STRUCT}
  | "SUB" {SUB}
  | "T---0"{TON}
  | "THEN" {THEN}
  | "TIME_OF_DAY" {TIME_OF_DAY}
  | "TIME" {TIME}
  | "TOF" {TOF}
  | "TON" {TON}
  | "TO" {TO}
  | "TOD" {TIME_OF_DAY}
  | "TP" {TP}
  | "TRUE" {TRUE}
  | "UDINT" {UDINT}
  | "UINT" {UINT}
  | "ULINT" {ULINT}
  | "UNTIL" {UNTIL}
  | "USINT" {USINT}
  | "VAR_ACCESS" {VAR_ACCESS}
  | "VAR_EXTERNAL" {VAR_EXTERNAL}
  | "VAR_GLOBAL" {VAR_GLOBAL}
  | "VAR_IN_OUT" {VAR_IN_OUT}
  | "VAR_INPUT" {VAR_INPUT}
  | "VAR_OUTPUT" {VAR_OUTPUT}
  | "VAR" {VAR}
  | "WHILE" {WHILE}
  | "WITH" {WITH}
  | "WORD" {WORD}
  | "XORN" {XORN}
  | "XOR" {XOR}
  | "//"_*'\n' {let () = new_line lexbuf in lexer lexbuf}
  | '\n' {let () = new_line lexbuf in lexer lexbuf}
  | ['0'-'9']+ {TInt (lexeme lexbuf)} (* an integer *)
  | ['0'-'9']+'.'['0'-'9']+ {TFloat (lexeme lexbuf)} (* a floating number *)
  | "<>" {OPER_NE}
  | "<=" {OPER_LE}
  | ">=" {OPER_GE}
  | "**" {OPER_EXP}
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* {IDENTIFIER (lexeme lexbuf)} (* any identifier a letter followed by anything, except a '$' sign*)
  | "(*" {comment 1 lexbuf} (* start of a comment *)
  | _  {lexer lexbuf} (* leave anything else *)
  | eof {TEof}
and comment depth = parse
  | "(*" {comment (depth + 1) lexbuf}
  | "*)" {if depth = 1 then lexer lexbuf else comment (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment depth lexbuf}
  | _ {comment depth lexbuf} 

{
(* The tail for now do nothing*)
}
