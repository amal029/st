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
%token <string> IDENTIFIER TInt TFloat

/* Constant constructors */
%token OPER_NE
%token OPER_LE
%token OPER_GE
%token OPER_EXP
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
%token END_CASE
%token END_FOR
%token END_FUNCTION
%token END_FUNCTION_BLOCK
%token END_PROGRAM
%token END_REPEAT
%token END_STEP
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
%token TEof

/* operator associative rules */

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
	  {St.op = Some (St.ST_CASE_ELEMENT); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}}
    | FALSE 
	{
	  let res = {St.t=Some St.BM_BOOL;St.leaf_type=Some St.LITERAL_INTEGER; St.literal_string= Some "0"; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.ST_CASE_ELEMENT); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}}
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
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | TIME {
	  let res = {St.t=Some St.BM_TIME;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
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
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | INT {
	  let res = {St.t=Some St.BM_INT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | DINT {
	  let res = {St.t=Some St.BM_DINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | LINT {
	  let res = {St.t=Some St.BM_LINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
;

unsigned_integer_type_name:
    | USINT {
	  let res = {St.t=Some St.BM_USINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | UINT {
	  let res = {St.t=Some St.BM_UINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | UDINT {
	  let res = {St.t=Some St.BM_UDINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | ULINT {
	  let res = {St.t=Some St.BM_ULINT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
;

real_type_name:
    | REAL {
	  let res = {St.t=Some St.BM_REAL;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | LREAL {
	  let res = {St.t=Some St.BM_LREAL;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
;

date_type_name:
    | DATE {
	  let res = {St.t=Some St.BM_DATE;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | TIME_OF_DAY {
	  let res = {St.t=Some St.BM_TOD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | DATE_AND_TIME {
	  let res = {St.t=Some St.BM_DT;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
;

bit_string_type_name:
    | BOOL {
	  let res = {St.t=Some St.BM_BOOL;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | BYTE {
	  let res = {St.t=Some St.BM_BYTE;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | WORD {
	  let res = {St.t=Some St.BM_WORD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | DWORD {
	  let res = {St.t=Some St.BM_DWORD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
	}
    | LWORD {
	  let res = {St.t=Some St.BM_LWORD;St.leaf_type=None; St.literal_string= None; St.leaf=None;
		     St.id=None; St.buff=None; St.leaf_data=None} in
	  {St.op = Some (St.TYPE_DEF); St.elements=[];St.result=Some res;St.n=0;St.location=Some (ln())}
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
	  let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in
	  let () = St.add_leaf ret (St.make_noop) in ret
	}
    | simple_specification ':' '=' constant {
			     let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
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
	  let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in
	  let () = St.add_leaf ret (St.make_noop) in ret
	}
    | subrange_specification ':' '=' signed_integer {
			       let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			       let () = St.add_leaf ret $1 in
			       let () = St.add_leaf ret $4 in ret
			     }
;

subrange_specification:
    | integer_type_name '(' subrange ')' {
			  let ret = {St.op = Some (St.TYPE_SPEC_SUBRANGE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			  let () = St.add_leaf ret $1 in
			  let () = St.add_leaf ret $3 in ret
			}
    | simple_type_name {
	  let ret = {St.op = Some (St.TYPE_SPEC_SUBRANGE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in
	  let () = St.add_leaf ret (St.make_noop) in ret
	}
;

subrange:
    | signed_integer '.' '.' signed_integer {St.make_literal_subrange ($1.St.result,$4.St.result)}
;

enumerated_spec_init:
    | simple_type_name ':' '=' simple_type_name {
	    let ret = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $1 in 
	    let () = St.add_leaf ret $4 in ret
	  }
    | simple_type_name {
	    let ret = {St.op = Some (St.TYPE_SPEC_ENUM); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $1 in
	    let () = St.add_leaf ret (St.make_noop) in ret
	}
;

enumerated_specification:
    | '(' enum_spec_10 ')' {
	    let ret = {St.op = Some (St.TYPE_SPEC_ENUM); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $2 in ret
	  }
    | simple_type_name {
	  let ret = {St.op = Some (St.TYPE_SPEC_ENUM); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in ret
	}
;

enum_spec_10:
    | simple_type_name {
	    let ret = {St.op = Some (St.TYPE_SPEC_ENUM_LIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $1 in ret
	}
    | enum_spec_10 ',' simple_type_name {
		     let () = St.add_leaf $1 $3 in $1
		   }
;

variable:
    | symbolic_variable {$1}
;

symbolic_variable:
    | simple_type_name {$1}
    | multi_element_variable {$1}
;

multi_element_variable:
    | array_variable {$1}
    | structured_variable {$1}
;

array_variable:
    | subscripted_variable '[' subscript_list ']' {
			     let ret = {St.op = Some (St.VARIABLE_ARRAY); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			     let () = St.add_leaf ret $1 in 
			     let () = St.add_leaf ret $3 in ret
			   }
;

subscripted_variable:
    | symbolic_variable {$1}
;

subscript_list:
    | subscript {
	  let ret = {St.op = Some (St.VARIABLE_ARRAY_SUBSCRIPT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in ret
	}
    | subscript_list ',' subscript {let () = St.add_leaf $1 $3 in $1}
;

subscript : 
    | expression {$1}
;

structured_variable:
    | symbolic_variable '.' simple_type_name {
			  let ret = {St.op = Some (St.VARIABLE_STRUCT_ELEMENT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			  let () = St.add_leaf ret $1 in 
			  let () = St.add_leaf ret $3 in ret
			}
;

/* TODO: Put the input declaration unto expressions here*/

input_declarations:
    | VAR_INPUT input_decl END_VAR {$2}
;

input_decl:
    | input_declaration ';' {
			  let ret = {St.op = Some (St.VARIABLE_INPUT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			  let () = St.add_leaf ret $1 in ret
			}
    | input_decl input_declaration ';' {
			  let () = St.add_leaf $1 $2 in $1
			}
;

input_declaration:
    | var_init_decl {$1}
    | edge_declaration {$1}
;

edge_declaration:
    | var1_list ':' BOOL R_EDGE {$1}
    | var1_list ':' BOOL F_EDGE {$1}
;

var_init_decl:
    | var1_init_decl {$1}
    | fb_name_decl {$1}
;

varinitdecl: 
    | var_init_decl ';' {
		      let ret = {St.op = Some (St.VARIABLE_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		      let () = St.add_leaf ret $1 in ret
		    }
    | varinitdecl var_init_decl ';' {
		    let () = St.add_leaf $1 $2 in $1
		 }
;

var1_init_decl:
    | var1_list ':' simple_spec_init {
		  let ret = {St.op = Some (St.VARIABLE_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.add_leaf ret $1 in
		  let () = St.add_leaf ret $3 in ret
		}
    | var1_list ':' subrange_spec_init {
		  let ret = {St.op = Some (St.VARIABLE_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.add_leaf ret $1 in
		  let () = St.add_leaf ret $3 in ret
		}
    | var1_list ':' enumerated_spec_init {
		  let ret = {St.op = Some (St.VARIABLE_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.add_leaf ret $1 in
		  let () = St.add_leaf ret $3 in ret
		}
;

var1_list:
    | simple_type_name {
	  let ret = {St.op = Some (St.VARIABLE_LIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in ret
	}
    | var1_list ',' simple_type_name {
		  let () = St.add_leaf $1 $3 in $1
		}
;

fb_name_decl:
    | var1_list ':' function_block_type_name {
		  let fb_tree = {St.op = Some (St.TYPE_SPEC); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.add_leaf fb_tree $3 in
		  let () = St.add_leaf fb_tree (St.make_noop) in
		  let ret = {St.op = Some (St.VARIABLE_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.add_leaf ret $1 in
		  let () = St.add_leaf ret fb_tree in 
		  let () = St.add_variables ret $1 $3 0 None in ret
		}
;

output_declarations:
    | VAR_OUTPUT varinitdecl END_VAR {
		  let ret = {St.op = Some (St.VARIABLE_OUTPUT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.transfer_leaf ret $2 in ret
		 }
    | VAR_OUTPUT RETAIN varinitdecl END_VAR {
		  let ret = {St.op = Some (St.VARIABLE_OUTPUT_RETAIN); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.transfer_leaf ret $3 in ret
		 }
;

input_output_declarations:
    | VAR_IN_OUT vardecl END_VAR {
		   let ret = {St.op = Some (St.VARIABLE_IN_OUT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		   let () = St.transfer_leaf ret $2 in ret
		 }
;

vardecl:
    | var_declaration ';' {
			let ret = {St.op = Some (St.VARIABLE_LIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			let () = St.add_leaf ret $1 in ret
		      }
    | vardecl var_declaration ';' {
		let ret = {St.op = Some (St.VARIABLE_LIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		let () = St.add_leaf ret $2 in ret
	      }
;

var_declaration:
    | var1_declaration {$1}
    | fb_name_decl {$1}
;

var1_declaration:
    | var1_list ':' simple_specification {$1}
    | var1_list ':' subrange_specification {$1}
    | var1_list ':' enumerated_specification {$1}
;

var_declarations:
    | VAR varinitdecl END_VAR {
	    let ret = {St.op = Some (St.VARIABLE_LOCAL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.transfer_leaf ret $2 in ret
	  }
    | VAR CONSTANT varinitdecl END_VAR {
	    let ret = {St.op = Some (St.VARIABLE_LOCAL_CONSTANT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.transfer_leaf ret $3 in ret
	  }
;

retentive_var_declarations: 
    | VAR RETAIN varinitdecl END_VAR {
	    let ret = {St.op = Some (St.VARIABLE_LOCAL_RETAIN); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.transfer_leaf ret $3 in ret
	  }
;

external_var_declarations:
    | VAR_EXTERNAL extdecl END_VAR {$2}
;

extdecl:
    | external_declaration ';' {
			     	    let ret = {St.op = Some (St.VARIABLE_EXTERNAL_LIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
				    let () = St.add_leaf ret $1 in ret
			   }
    | extdecl external_declaration ';' {
		let () = St.add_leaf $1 $2 in $1
	      }
;

external_declaration:
    | simple_type_name ':' simple_specification {
				    let ret = {St.op = Some (St.VARIABLE_EXTERNAL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
				    let () = St.add_leaf ret $1 in 
				    let () = St.add_leaf ret $3 in ret
		      }
    | simple_type_name ':' subrange_specification {
			let ret = {St.op = Some (St.VARIABLE_EXTERNAL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			let () = St.add_leaf ret $1 in 
			let () = St.add_leaf ret $3 in ret
		      }
    | simple_type_name ':' enumerated_specification {
			let ret = {St.op = Some (St.VARIABLE_EXTERNAL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			let () = St.add_leaf ret $1 in 
			let () = St.add_leaf ret $3 in ret
		      }
;

function_name: 
    | std_func_name1 {$1}
    | std_func_name2 {$1}
    | simple_type_name {$1}
;

std_func_name1:
    | MOD {St.make_literal_id "MOD"}
    | EXPT {St.make_literal_id "EXPT"}
    | MOVE {St.make_literal_id "MOVE"}
;

std_func_name2:
    | AND {St.make_literal_id "AND" }
    | OR {St.make_literal_id "OR" }
    | XOR {St.make_literal_id "XOR" }
    | ADD {St.make_literal_id "ADD" }
    | SUB {St.make_literal_id "SUB" }
    | MUL {St.make_literal_id "MUL" }
    | DIV {St.make_literal_id "DIV" }
    | GT {St.make_literal_id "GT" }
    | LT {St.make_literal_id "LT" }
    | GE {St.make_literal_id "GE" }
    | LE {St.make_literal_id "LE" }
    | EQ {St.make_literal_id "EQ" }
    | NE {St.make_literal_id "NE" }
;

function_declaration:
    | function_declare_name input_declarations funcvardecl function_body END_FUNCTION {
			      let () = St.add_leaf $1 $2 in
			      let () = St.add_leaf $1 $3 in
			      let () = St.add_leaf $1 $4 in $1
			    }
;

function_declare_name:
    | FUNCTION simple_type_name ':' elementary_type_name {
		 let ret = {St.op = Some (St.FUNCTION_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		 let () = St.add_leaf ret $2 in
		 let () = St.add_leaf ret $4 in
		 let () = St.add_variable $2.result.id $4 0 None in ret
	       }
    | FUNCTION simple_type_name ':' derived_type_name {
		 let ret = {St.op = Some (St.FUNCTION_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		 let () = St.add_leaf ret $2 in
		 let () = St.add_leaf ret $4 in
		 let () = St.add_variable $2.result.id $4 0 None in ret
	       }
;

funcvardecl:
    | VAR function_var_decls END_VAR {
	    let ret = {St.op = Some (St.VARIABLE_LOCAL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $2 in
	    let () = St.add_leaf ret (St.make_literal_int "0") in ret
	  }
    | VAR CONSTANT function_var_decls END_VAR {
	    let ret = {St.op = Some (St.VARIABLE_LOCAL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $3 in
	    let () = St.add_leaf ret (St.make_literal_int "1") in ret
	  }
;

/* Is this correct ?? */
function_var_decls: 
    | function_var_decl ';' {$1}
    | function_var_decls function_var_decl ';' {$1}
;

function_var_decl: 
    | var1_declaration {$1}
;

function_body: 
    | statement_list {$1}
;

function_block_type_name:
    | simple_type_name {
	  $1.op <- St.TYPE_DEF;
	  let () = $1.result.t <- St.funcblk_type $1.result.id in $1
	}
    | standard_function_block_name {
	  $1.op <- ST.TYPE_DEF;
	  let () = $1.result.t <- St.funcblk_type $1.result.id in $1
	}
;

standard_function_block_name:
    | SR {St.make_literal_id "ST"}
    | R_TRIG {St.make_literal_id "R_TRIG"}
    | F_TRIG {St.make_literal_id "F_TRIG"}
    | CTU {St.make_literal_id "CTU"}
    | CTD {St.make_literal_id "CTD"}
    | CTUD {St.make_literal_id "CTUD"}
    | TP {St.make_literal_id "TP"}
    | TON {St.make_literal_id "TON"}
    | TOF {St.make_literal_id "TOF"}
;

function_block_declaration:
    | FUNCTION_BLOCK simple_type_name fb_io_var_decls other_var_decls function_block_body END_FUNCTION_BLOCK 
		     {
		       let ret = {St.op = Some (St.FB_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		       let () = St.add_leaf ret $2 in
		       let () = St.add_leaf ret $3 in
		       let () = St.add_leaf ret $4 in
		       let () = St.add_leaf ret $5 in ret
		     }
    | FUNCTION_BLOCK standard_function_block_name fb_io_var_decls other_var_decls function_block_body END_FUNCTION_BLOCK 
		     {
		       let ret = {St.op = Some (St.FB_DECLARE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		       let () = St.add_leaf ret $2 in
		       let () = St.add_leaf ret $3 in
		       let () = St.add_leaf ret $4 in
		       let () = St.add_leaf ret $5 in ret
		     }
;

fb_io_var_decls:
    | {St.make_noop}
    | fb_io_var_decls fb_io_var_declarations {St.add_leaf $1 $2; $1}
;

other_var_decls: 
    | {St.make_noop}
    | other_var_decls other_var_declarations {St.add_leaf $1 $2; $1}

fb_io_var_declarations:
    | input_declarations {$1}
    | output_declarations {$1}
    | input_output_declarations {$1}
;

other_var_declarations:
    | external_var_declarations {$1}
    | var_declarations {$1}
    | retentive_var_declarations {$1}
;

function_block_body:
    | statement_list {$1}
;

expression:
    | xor_expression {$1}
    | expression OR xor_expression {
		   let () = St.concat_leaf $1 (St.EXPR_XOR) in
		   let () = St.add_leaf $1 $3 in $1
		 }
;

xor_expression:
    | xor_expression XOR and_expression {
		       let () = St.concat_leaf $1 (St.EXPR_XOR) in
		       let () = St.add_leaf $1 $3 in $1
		     }
;

and_expression:
    | and_expression '&' comparison {
		       let () = St.concat_leaf $1 (St.EXPR_AND) in
		       let () = St.add_leaf $1 $3 in $1
		     }
    | and_expression AND comparison {
		       let () = St.concat_leaf $1 (St.EXPR_AND) in
		       let () = St.add_leaf $1 $3 in $1
		     }
;

comparison:
    | equ_expression {$1}
    | comparison '=' equ_expression {
		   let () = St.concat_leaf $1 (St.EXPR_EQ) in
		   let () = St.add_leaf $1 $3 in $1
		 }
    | comparison OPER_NE equ_expression {
		   let () = St.concat_leaf $1 (St.EXPR_NE) in
		   let () = St.add_leaf $1 $3 in $1
		 }
;

equ_expression:
    | equ_expression '<' add_expression {
		       let () = St.concat_leaf $1 (St.EXPR_LT) in
		       let () = St.add_leaf $1 $3 in $1
		     }
    | equ_expression '>' add_expression {
		       let () = St.concat_leaf $1 (St.EXPR_GT) in
		       let () = St.add_leaf $1 $3 in $1
		     }
    | equ_expression OPER_LE add_expression {
		       let () = St.concat_leaf $1 (St.EXPR_LE) in
		       let () = St.add_leaf $1 $3 in $1
		     }
    | equ_expression OPER_GE add_expression {
		       let () = St.concat_leaf $1 (St.EXPR_GE) in
		       let () = St.add_leaf $1 $3 in $1
		     }

;

add_expression:
    | term {$1}
    | add_expression '+' term {
		   let () = St.concat_leaf $1 (St.EXPR_ADD) in
		   let () = St.add_leaf $1 $3 in $1
		     }
    | add_expression '-' term {
		   let () = St.concat_leaf $1 (St.EXPR_SUB) in
		   let () = St.add_leaf $1 $3 in $1
		     }
;

term:
    | power_expression {$1}
    | term '*' power_expression {
		   let () = St.concat_leaf $1 (St.EXPR_MUL) in
		   let () = St.add_leaf $1 $3 in $1
	   }
    | term '/' power_expression {
	     let () = St.concat_leaf $1 (St.EXPR_DIV) in
	     let () = St.add_leaf $1 $3 in $1
	   }
    | term MOD power_expression {
	     let () = St.concat_leaf $1 (St.EXPR_MOD) in
	     let () = St.add_leaf $1 $3 in $1
	   }
;

power_expression:
    | unary_expression {$1}
    | power_expression OPER_EXP unary_expression {
			 let () = St.concat_leaf $1 (St.EXPR_POW) in
			 let () = St.add_leaf $1 $3 in $1
		       }
;

unary_expression:
    | primary_expression {$1}
    | '-' primary_expression {
	    let ret = {St.op = Some (St.EXPR_UNARY); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $2 in ret
	  }
    | NOT primary_expression {
	    let ret = {St.op = Some (St.EXPR_NOT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $2 in ret
	  }
;

primary_expression:
    | constant {$1}
    | variable {$1}
    | '(' expression ')' {$2}
    | function_name '(' ')' {
		      let ret = {St.op = Some (St.FUNC_CALL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		      let () = St.add_leaf ret $1 in 
		      let () = St.add_leaf ret (St.make_noop) in ret
		    }
    | function_name '(' fb_input_assignments ')' {
		      let ret = {St.op = Some (St.FUNC_CALL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		      let () = St.add_leaf ret $1 in 
		      let () = St.add_leaf ret $3 in ret
		    }
    | simple_type_name '.' simple_type_name {
		      let ret = {St.op = Some (St.FB_VARIABLE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		      let () = St.add_leaf ret $1 in 
		      let () = St.add_leaf ret $3 in ret
	      }
    | simple_type_name '.' variable {
		      let ret = {St.op = Some (St.FB_VARIABLE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		      let () = St.add_leaf ret $1 in 
		      let () = St.add_leaf ret $3 in ret
	      }
;

statement_list:
    | statement ';' {
		  let ret = {St.op = Some (St.ST_STATEMENT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		  let () = St.add_leaf ret $1 in ret
		}
    | statement_list statement ';' {
		       let () = St.add_leaf $1 $2 in $1
		     }
;

statement:
    | assignment_statement {$1}
    | subprogram_control_statement {$1}
    | selection_statement {$1}
    | iteration_statement {$1}
;

assignment_statement:
    | variable ':' '=' expression {
		 let ret = {St.op = Some (St.ST_ASSIGNMENT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		 let () = St.add_leaf ret $1 in 
		 let () = St.add_leaf ret $4 in ret
	       }
;

subprogram_control_statement:
    | fb_invocation {$1}
    | RETURN {{St.op = Some (St.ST_RETURN); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} }
;

fb_invocation:
    | simple_type_name '(' ')' {
			 let ret = {St.op = Some (St.FB_CALL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			 let () = St.add_leaf ret $1 in 
			 let () = St.add_leaf ret (St.make_noop) in
			 let () = St.add_leaf ret (St.make_literal_int "0") in ret
		       }
    | simple_type_name '(' fb_input_assignments ')' {
			 let ret = {St.op = Some (St.FB_CALL); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			 let () = St.add_leaf ret $1 in 
			 let () = St.add_leaf ret $3 in
			 let () = St.add_leaf ret (St.make_literal_int "0") in ret
		       }
;

fb_input_assignments:
    | fb_input_assignment {
		 let ret = {St.op = Some (St.FB_VARLIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		 let () = St.add_leaf ret $1 in ret
	}
    | fb_input_assignments ',' fb_input_assignment {
			     let () = St.add_leaf $1 $3 in $1
			   }
;

/* Check this later, might give reduce/reduce conflict with assignment statement */
fb_input_assignment:
    | simple_type_name ':' '=' expression {
			 let ret = {St.op = Some (St.FB_VAR); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			 let () = St.add_leaf ret $1 in
			 let () = St.add_leaf ret $4 in ret
		       }
    | expression {
			 let ret = {St.op = Some (St.FB_VAR); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
			 let () = St.add_leaf ret (St.make_noop) in
			 let () = St.add_leaf ret $1 in ret
	}
;

selection_statement:
    | case_statement {$1}
    | if_statement {$1}
;

if_statement:
    | IF expression THEN statement_list elsif_list END_IF
	 {
	   let ret = {St.op = Some (St.ST_IF_STMT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	   let () = St.add_leaf ret $2 in
	   let () = St.add_leaf ret $4 in
	   let () = St.add_leaf ret $5 in
	   let () = St.add_leaf ret (St.make_noop) in ret
	 }
    | IF expression THEN statement_list elsif_list ELSE statement_list END_IF
	 {
	   let ret = {St.op = Some (St.ST_IF_STMT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	   let () = St.add_leaf ret $2 in
	   let () = St.add_leaf ret $4 in
	   let () = St.add_leaf ret $5 in
	   let () = St.add_leaf ret $7 in ret
	 }
;

elsif_list:
    | ELSIF expression THEN statement_list 
		 {
		   let ret = {St.op = Some (St.ST_IF_ELSIF); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
		   let () = St.add_leaf ret $2 in
		   let () = St.add_leaf ret $4 in ret
		 }
    | elsif_list ELSIF expression THEN statement_list 
		 {
		   let () = St.add_leaf $1 $3 in
		   let () = St.add_leaf $1 $5 in $1
		 }
;

case_statement:
    | CASE expression OF case_elements END_CASE {
	     let ret = {St.op = Some (St.ST_CASE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	     let () = St.add_leaf ret $2 in
	     let () = St.add_leaf ret (St.make_noop) in
	     let () = St.add_leaf ret $4 in ret
	   }
    | CASE expression OF case_elements ELSE statement_list END_CASE 
	   {
	     let ret = {St.op = Some (St.ST_CASE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	     let () = St.add_leaf ret $2 in
	     let () = St.add_leaf ret $4 in
	     let () = St.add_leaf ret $6 in ret
	   }
;

case_elements:
    | case_list ':' statement_list {
		  let ret = {St.op = Some (St.ST_CASE_ELEMENT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())}in 
		  let () = St.add_leaf ret $1 in
		  let () = St.add_leaf ret $3 in ret
		}
    | case_elements case_list ':' statement_list {St.add_leaf $1 $2; St.add_leaf $1 $4}
;

case_list:
    | subrange {
	  let ret = {St.op = Some (St.ST_CASE_LIST); St.elements=[$1];St.result=None;St.n=0;St.location=Some (ln())} in
	  let () = St.add_leaf ret $1 in ret
	}
    | signed_integer {
	  let ret = {St.op = Some (St.ST_CASE_LIST); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
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
	    let ret = {St.op = Some (St.ST_FOR_LOOP); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	    let () = St.add_leaf ret $2 in
	    let () = St.add_leaf ret $5 in
	    let () = St.add_leaf ret $7 in
	    let () = St.add_leaf ret lit_int in
	    let () = St.add_leaf ret $9 in
	    ret
	  }
    | FOR simple_type_name ':' '=' expression TO expression BY expression DO statement_list END_FOR
	  {
	    let ret = {St.op = Some (St.ST_FOR_LOOP); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
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
	      let ret = {St.op = Some (St.ST_WHILE); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())} in
	      let () = St.add_leaf ret $2 in
	      let () = St.add_leaf ret $4 in
	      ret
	    }
;

repeat_statement:
    | REPEAT statement_list UNTIL expression END_REPEAT 
	     {
	       let ret = {St.op = Some (St.ST_REPEAT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())}  in
	       let () = St.add_leaf ret $2 in
	       let () = St.add_leaf ret $4 in
	       ret
	     }
;

exit_statement:
    | EXIT {{St.op = Some (St.ST_EXIT); St.elements=[];St.result=None;St.n=0;St.location=Some (ln())}}
;

%%
(* This is the trailer *)
