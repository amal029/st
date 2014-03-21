(* 
   Author: Avinash Malik
   The IEC61131-3 Structured text language.
Thu Mar 20 17:22:33 NZDT 2014
*)

open Sexplib.Std
open Sexplib.Sexp
module L = Batteries.List
module String = Batteries.String

let (|>) x f = f x

exception Internal_error of string

type data_type = 
  | BM_SINT         
  | BM_INT          
  | BM_DINT         
  | BM_LINT         
  | BM_USINT        
  | BM_UINT         
  | BM_UDINT        
  | BM_ULINT        
  | BM_ANY_INT      
  | BM_BOOL         
  | BM_BYTE         
  | BM_WORD         
  | BM_DWORD        
  | BM_LWORD        
  | BM_ANY_BIT      
  | BM_REAL         
  | BM_LREAL        
  | BM_ANY_REAL
  | BM_STRING       
  | BM_TIME         
  | BM_DATE         
  | BM_TOD          
  | BM_DT           
  | BM_ANY     
  with sexp

type op_code =
  | EXPR_ADD
  | EXPR_SUB
  | EXPR_DIV
  | EXPR_MOD
  | EXPR_AND
  | EXPR_NE
  | EXPR_EQ
  | EXPR_LT
  | EXPR_GT
  | EXPR_LE
  | EXPR_GE
  | EXPR_MUL	
  | EXPR_NOT
  | EXPR_OR
  | EXPR_POW
  | EXPR_UNARY
  | EXPR_XOR
  | FB_CALL
  | FB_DECLARE
  | FB_VAR
  | FB_VARIABLE
  | FB_VARLIST
  | FUNC_CALL
  | FUNCTION_DECLARE
  | LITERAL
  | NOOP
  | ST_ASSIGNMENT
  | ST_CASE
  | ST_CASE_ELEMENT
  | ST_CASE_LIST
  | ST_EXIT	
  | ST_FOR_LOOP
  | ST_IF_ELSIF
  | ST_IF_STMT
  | ST_REPEAT
  | ST_RETURN
  | ST_STATEMENT
  | ST_WHILE
  | TYPE_ARRAY_INITIAL_LIST
  | TYPE_ARRAY_SUBRANGELIST
  | TYPE_DECLARE
  | TYPE_DECLARE_LIST	
  | TYPE_DECLARE_STRUCT
  | TYPE_DEF
  | TYPE_SPEC
  | TYPE_SPEC_ARRAY
  | TYPE_SPEC_ENUM
  | TYPE_SPEC_ENUM_LIST
  | TYPE_SPEC_SUBRANGE
  | TYPE_STRUCT_DECLARE
  | TYPE_STRUCT_ELEMENT
  | TYPE_STRUCT_INITIALISATION
  | TYPE_STRUCT_INITIALISED
  | VARIABLE_ARRAY
  | VARIABLE_ARRAY_SUBSCRIPT
  | VARIABLE_DECLARE
  | VARIABLE_DECLARE_LIST
  | VARIABLE_EXTERNAL
  | VARIABLE_EXTERNAL_LIST
  | VARIABLE_GLOBAL
  | VARIABLE_GLOBAL_LIST
  | VARIABLE_IN_OUT
  | VARIABLE_INPUT
  | VARIABLE_LIST
  |VARIABLE_LOCAL
  |VARIABLE_LOCAL_CONSTANT
  |VARIABLE_LOCAL_RETAIN
  |VARIABLE_LOCATED
  |VARIABLE_LOCATEDLIST
  |VARIABLE_OUTPUT
  |VARIABLE_OUTPUT_RETAIN
  |VARIABLE_STRING
  |VARIABLE_STRUCT_ELEMENT
  with sexp

type parse_tree_data =
  | TREE_LEAF
  | LITERAL_INTEGER
  | LITERAL_REAL
  | LITERAL_STRING
  | LITERAL_SUBRANGE
  | LITERAL_ID
  with sexp

type leaf_data = 
  | Int of int
  | Real of float
  | Array of (int * int)
  with sexp

type parse_leaf = {
  mutable t:data_type option;
  mutable leaf_type:parse_tree_data option;
  mutable leaf:parse_tree option;
  mutable leaf_data:leaf_data option;
  mutable literal_string:string option;
  mutable id:string option;
  mutable buff:string option;
}
 and parse_tree = {
   mutable op:op_code option;
   mutable result:parse_leaf option;
   mutable n:int;
   mutable elements: parse_leaf list;
   location:(int * int) option;
 } with sexp


let add_leaf dest src = 
  dest.n <- dest.n + 1;
  let src_t = (function Some x -> x.t | None -> None ) src.result in
  let toadd = {t=src_t;leaf_type=Some TREE_LEAF;leaf=Some src;
	      leaf_data=None; literal_string=None;id=None;
	      buff=None} in
  dest.elements <- dest.elements @ [toadd]

(* TODO let transfer_leaf dest src =  *)
(* TODO Need to write all the add functions and then we are done *)

let make_noop = 
  {op=Some NOOP;result=None;n=0;elements=[];location=None}

let make_literal_int s = 
  let result = {t=Some BM_ANY_INT;leaf_type=Some LITERAL_INTEGER;leaf_data=Some (Int s);leaf=None;
	       literal_string=None; id=None; buff=None} in
  {op=Some LITERAL;result=Some result;n=0;elements=[];location=None}


let make_literal_real s idt = 
  let result = {t=Some idt;leaf_type=Some LITERAL_REAL;leaf_data=Some (Real s);leaf=None;
	       literal_string=None; id=None; buff=None} in
  {op=Some LITERAL;result=Some result;n=0;elements=[];location=None}

let make_literal_string s = 
  let result = {t=Some BM_STRING;leaf_type=Some LITERAL_STRING;leaf_data=None;leaf=None;
	       literal_string=Some s; id=None; buff=None} in
  {op=Some LITERAL;result=Some result;n=0;elements=[];location=None}

let make_literal_id s = 
  let result = {t=None;leaf_type=Some LITERAL_ID;leaf_data=None;leaf=None;
	       literal_string=None; id=Some s; buff=None} in
  {op=Some LITERAL;result=Some result;n=0;elements=[];location=None}

let make_literal_subrange lo hi = 
  let result = {t=None;leaf_type=Some LITERAL_SUBRANGE;leaf_data=Some (Array (lo,hi));leaf=None;
	       literal_string=None; id=None; buff=None} in
  {op=Some LITERAL;result=Some result;n=0;elements=[];location=None}

let concat_leaf src op = 
  if (match src.op with Some x -> x | None -> raise (Internal_error (("Source parse tree has type Opcode: " 
								      ^ (to_string_hum (sexp_of_parse_tree src)))))) = op then src
  else 
    let ret = {op=Some op;result=None;n=0;elements=[];location=None} in
    add_leaf ret src;
    ret

  
  
  
