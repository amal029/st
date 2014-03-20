%{
(* This is the header *)
(* let _ = Parsing.set_trace true in () *)
 let counter = ref 0
 let line_nums = Hashtbl.create (1000) 
 let parse_error s = 
   let mypos = Parsing.symbol_start_pos() in
   print_string (s ^ " (line number: ");
   print_int mypos.Lexing.pos_lnum;
   print_string (", column number: " ^ (string_of_int (mypos.Lexing.pos_cnum - mypos.Lexing.pos_bol)));
   print_endline ")";
   flush stdout
 let ln () = 
   let mypos = Parsing.symbol_start_pos() in 
   let cnum = (mypos.Lexing.pos_cnum) - (mypos.Lexing.pos_bol) in
   (mypos.Lexing.pos_lnum,cnum)
%}

/* Constructors with an argument */
%token <int> Integer Binary_integer Octal_integer Hex_integer
%token <float> Real_literal Duration Time_of_day Date Date_and_time
%token <string> Character_string
%token <string> IDENTIFIER
%token <string> Simple_type_name Subrange_type_name Enumerated_type_name
%token <string> Array_type_name Structure_type_name String_type_name Variable_name 
%token <string> Global_var_name Fb_name
%token <string> Oper_exp Oper_ne Oper_le Oper_ge

/* Constant constructors */
%token ACTION
%token ADD
%token AND
%token ANDN
%token ARRAY
%token AT
%token BOOL
%token BY
%token BYTE
%token CAL
%token CALC
%token CALCN
%token CASE
%token CD
%token CLK
%token CONCAT
%token CONSTANT
%token CONFIGURATION
%token CTU
%token CTD
%token CTUD
%token CU
%token DATE
%token DATE_AND_TIME
%token DINT
%token DIV
%token DO
%token DWORD
%token END_ACTION
%token END_CONFIGURATION
%token END_CASE
%token END_FOR
%token END_FUNCTION
%token END_FUNCTION_BLOCK
%token END_PROGRAM
%token END_REPEAT
%token END_STEP
%token END_TYPE
%token END_RESOURCE
%token END_STRUCT
%token END_TRANSITION
%token END_VAR
%token END_WHILE
%token END_IF
%token ELSE
%token ELSIF
%token EQ
%token EXIT
%token EXPT
%token F_EDGE
%token F_TRIG
%token FALSE
%token FOR
%token FROM
%token FUNCTION
%token FUNCTION_BLOCK
%token GE
%token GT
%token IF
%token IN
%token INSERT
%token INT
%token INITIAL_STEP
%token INTERVAL
%token JMP
%token JMPC
%token JMPCN
%token LD
%token LDN
%token LE
%token LIMIT
%token LT
%token LINT
%token LREAL
%token LWORD
%token MAX
%token MIN
%token MOD
%token MOVE
%token MUL
%token MUX
%token NE
%token NOT
%token OF
%token ON
%token OR
%token ORN
%token PROGRAM
%token PRIORITY
%token PT
%token PV
%token R_EDGE
%token R_TRIG
%token R1
%token READ_ONLY
%token READ_WRITE
%token REAL
%token REPEAT
%token RESOURCE
%token RET
%token RETAIN
%token RETC
%token RETCN
%token RETURN
%token RS
%token S1
%token SEL
%token SINGLE
%token SINT
%token SQRT
%token SR
%token ST
%token STEP
%token STN
%token STRING
%token STRUCT
%token SUB
%token TASK
%token THEN
%token TIME
%token TIME_OF_DAY
%token TO
%token TOF
%token TON
%token TP
%token TRANSITION
%token TRUE
%token TYPE
%token UDINT
%token UINT
%token ULINT
%token UNTIL
%token USINT
%token VAR
%token VAR_ACCESS
%token VAR_EXTERNAL
%token VAR_GLOBAL
%token VAR_IN_OUT
%token VAR_INPUT
%token VAR_OUTPUT
%token WHILE
%token WITH
%token WORD
%token XOR
%token XORN

/* operator associative rules */
%left TRShift TLShift
%left TPlus TMinus
%left TTimes TDiv TMod
%left TPow
%left TOP TCP
%left And Or
%left TXCL
%nonassoc TUminus /* useful for difference between -2 and 1 - 2*/

/* The start of the parsing function */
%start start
%type <St.parse_tree> start

%%
/* These are the parsing rules */

start:
    | TEof {St.make_noop}
    | function_block_declaration TEof {$1}
;

constant:
    | numeric_literal {$1}
    | '"' IDENTIFIER '"' {St.make_literal_string $2}
;

numeric_literal:
    | integer_literal {$1}
    | TFloat {St.make_literal_real $1 St.BM_ANY_REAL} /*This is the real_literal, only float type are supported for now!*/
    | TRUE {
	  let res = {St.t=Some St.BM_BOOL;St.leaf_type=Some St.LITERAL_INTEGER; St.literal_string= Some "1"; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.ST_CASE_ELEMENT); St.elements=[];St.result=res;St.n=0;St.location=ln()}}
    | FALSE 
	{
	  let res = {St.t=Some St.BM_BOOL;St.leaf_type=Some St.LITERAL_INTEGER; St.literal_string= Some "0"; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.ST_CASE_ELEMENT); St.elements=[];St.result=res;St.n=0;St.location=ln()}}
;

integer_literal:
    | signed_integer {$1}
;

signed_integer:
    | TInt {St.make_literal_int $1}
    | '+' TInt {St.make_literal_int $2}
    | '-' TInt {St.make_literal_int ("-" ^ $2)}
;

elementary_type_name:
    | numeric_type_name {$1}
    | date_type_name {$1}
    | STRING {
	  let res = {St.t=Some St.BM_STRING;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | TIME {
	  let res = {St.t=Some St.BM_TIME;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | bit_string_type_name {$1}
;

numeric_type_name:
    | integer_type_name {$1}
    | real_type_name {$1}
;

integer_type_name:
    | signed_integer_type_name {$1}
    | unsigned_integer_type_name {$1}
;

signed_integer_type_name:
    | SINT {
	  let res = {St.t=Some St.BM_SINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | INT {
	  let res = {St.t=Some St.BM_INT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | DINT {
	  let res = {St.t=Some St.BM_DINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | LINT {
	  let res = {St.t=Some St.BM_LINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
;

unsigned_integer_type_name:
    | USINT {
	  let res = {St.t=Some St.BM_USINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | UINT {
	  let res = {St.t=Some St.BM_UINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | UDINT {
	  let res = {St.t=Some St.BM_UDINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | ULINT {
	  let res = {St.t=Some St.BM_ULINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
;

real_type_name:
    | REAL {
	  let res = {St.t=Some St.BM_REAL;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | LREAL {
	  let res = {St.t=Some St.BM_LREAL;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
;

date_type_name:
    | DATE {
	  let res = {St.t=Some St.BM_DATE;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | TIME_OF_DAY {
	  let res = {St.t=Some St.BM_TOD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | DATE_AND_TIME {
	  let res = {St.t=Some St.BM_DT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
;

bit_string_type_name:
    | BOOL {
	  let res = {St.t=Some St.BM_BOOL;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | BYTE {
	  let res = {St.t=Some St.BM_BYTE;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | WORD {
	  let res = {St.t=Some St.BM_WORD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | DWORD {
	  let res = {St.t=Some St.BM_DWORD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
    | LWORD {
	  let res = {St.t=Some St.BM_LWORD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=res;St.n=0;St.location=ln()}
	}
;

derived_type_name:
    | simple_type_name {$1}
;

simple_type_name:
    | IDENTIFIER {St.make_literal_id $1}
;

simple_spec_init:
    | simple_specification {
	  let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	  let () = St.add_leaf ret $1 in
	  let () = St.add_leaf ret (St.make_noop) in ret
	}
    | simple_specification ':' '=' constant {
			     let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
			     let () = St.add_leaf ret $1 in
			     let () = St.add_leaf ret $4 in ret
			   }
;

simple_specification:
    | elementary_type_name {$1}
    | simple_type_name {$1}
;

subrange_spec_init:
    | subrange_specification {
	  let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	  let () = St.add_leaf ret $1 in
	  let () = St.add_leaf ret (St.make_noop) in ret
	}
    | subrange_specification ':' '=' signed_integer {
			       let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
			       let () = St.add_leaf ret $1 in
			       let () = St.add_leaf ret $4 in ret
			     }
;

subrange_specification:
    | integer_type_name '(' subrange ')' {
			  let ret = {St.op = Some (St.TYPE_SPEC_SUBRANGE); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
			  let () = St.add_leaf ret $1 in
			  let () = St.add_leaf ret $3 in ret
			}
    | subrange_type_name {
	  let ret = {St.op = Some (St.TYPE_SPEC_SUBRANGE); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	  let () = St.add_leaf ret $1 in
	  let () = St.add_leaf ret (St.make_noop) in ret
	}
;

subrange:
    | signed_integer '.' '.' signed_integer {St.make_literal_subrange $1 $4}
;

case_statement:
    | CASE expression OF case_elements END_CASE {
	     let ret = {St.op = Some (St.ST_CASE); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	     let () = St.add_leaf ret $1 in
	     let () = St.add_leaf ret (St.make_noop) in
	     let () = St.add_leaf ret $4 in ret
	   }
    | CASE expression OF case_elements ELSE statement_list END_CASE 
	   {
	     let ret = {St.op = Some (St.ST_CASE); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	     let () = St.add_leaf ret $1 in
	     let () = St.add_leaf ret $2 in
	     let () = St.add_leaf ret $4 in ret
	   }
;

case_elements:
    | case_list ':' statement_list {
		  let ret = {St.op = Some (St.ST_CASE_ELEMENT); St.elements=[];St.result=None;St.n=0;St.location=ln()}in 
		  let () = St.add_leaf ret $1 in
		  let () = St.add_leaf ret $3 in ret
		}
    | case_elements case_list ':' statement_list {St.add_leaf $1 $2; St.add_leaf $1 $4}
;

case_list:
    | subrange {
	  let ret = {St.op = Some (St.ST_CASE_LIST); St.elements=[$1];St.result=None;St.n=0;St.location=ln()} in
	  let () = St.add_leaf ret $1 in ret
	}
    | signed_integer {
	  let ret = {St.op = Some (St.ST_CASE_LIST); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	  let () = St.add_leaf ret $1 in ret
	}
    | case_list ',' subrange {St.add_leaf $1 $3}
    | case_list ',' signed_integer {St.add_leaf $1 $3}
;

iteration_statement:
    | for_statement {$1}
    | while_statement {$1}
    | repeat_statement {$1}
    | exit_statement {$1}
;

for_statement:
    | FOR simple_type_name ':' '=' expression TO expression DO statement_list END_FOR
	  {
	    let lit_int = {St.t=Some St.BM_ANY_INT;St.leaf_type=St.LITERAL_INTEGER;St.leaf=None;St.literal_string=Some "1";St.id=None;St.buff=None} in
	    let ret = {St.op = Some (St.ST_FOR_LOOP); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	    let () = St.add_leaf ret $2 in
	    let () = St.add_leaf ret $5 in
	    let () = St.add_leaf ret $7 in
	    let () = St.add_leaf ret lit_int in
	    let () = St.add_leaf ret $9 in
	    ret
	  }
    | FOR simple_type_name ':' '=' expression TO expression BY expression DO statement_list ENF_FOR
	  {
	    let ret = {St.op = Some (St.ST_FOR_LOOP); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	    let () = St.add_leaf ret $2 in
	    let () = St.add_leaf ret $5 in
	    let () = St.add_leaf ret $7 in
	    let () = St.add_leaf ret $9 in
	    let () = St.add_leaf ret $11 in
	    ret
	  }
;

while_statement:
    | WHILE expression DO statement_list END_WHILE
	    {
	      let ret = {St.op = Some (St.ST_WHILE); St.elements=[];St.result=None;St.n=0;St.location=ln()} in
	      let () = St.add_leaf ret $2 in
	      let () = St.add_leaf ret $4 in
	      ret
	    }
;

repeat_statement:
    | REPEAT statement_list UNTIL expression END_REPEAT 
	     {
	       let ret = {St.op = Some (St.ST_REPEAT); St.elements=[];St.result=None;St.n=0;St.location=ln()}  in
	       let () = St.add_leaf ret $2 in
	       let () = St.add_leaf ret $4 in
	       ret
	     }
;

exit_statement:
    | EXIT {{St.op = Some (St.ST_EXIT); St.elements=[];St.result=None;St.n=0;St.location=ln()}}
;

%%
(* This is the trailer *)
