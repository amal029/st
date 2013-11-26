(* 
   Author: Avinash Malik
   The IEC61131-3 Structured text language
   Tue Nov 26 15:00:38 NZDT 2013
*)

open Sexplib.Std
module List = Batteries.List
module String = Batteries.String

type line = int
with sexp
type column = int
with sexp

type _identifier = string
with sexp

type generic_type_name =
| ANY
| ANY_DERIVED
| ANY_ELEMENTARY
| ANY_MAGNITUDE
| ANY_NUM
| ANY_INT
| ANY_BIT
| ANY_STRING
| ANY_DATE
with sexp

type simple_type_name = _identifier
with sexp
type subrange_type_name = _identifier
with sexp
type enumerated_type_name = _identifier
with sexp
type array_type_name = _identifier
with sexp
type structure_type_name = _identifier
with sexp
type string_type_name = _identifier
with sexp
type structure_element_name = _identifier
with sexp

type pointer_to = bool
with sexp

(* B.1.1 *)
type letter = string
with sexp
type digit = string
with sexp
type octal_digit = string
with sexp
type hex_digit = string
with sexp

(* B.1.2 *)

type _constant = 
| Time_literal of time_literal
| Numeric_literal of _numeric_literal
| Character_string of _character_string
| Bit_string_literal of string
| Boolean_literal of string
and bit_string_literal = 
| Bool of string
| Byte of string
| Word of string
| DWord of string
| LWord of string
(* B.1.2.1 *)
and _numeric_literal =
| RL of real_literal
| IL of integer_literal
and integer_literal = (integer_type_name * string)
and signed_integer = string
and integer = string
and binary_integer = string
and octal_integer = string
and hex_integer = string
and real_literal = (real_type_name * string)
and exponent = string

(* B.1.2.2 *)
and _character_string = 
| Single_byte_character_string of single_byte_character_string
| Double_byte_character_string of double_byte_character_string
| Single_byte_character_representation of single_byte_character_representation
| Double_byte_character_representation of double_byte_character_representation
| Common_character_representation of common_character_representation

and single_byte_character_string = string
and double_byte_character_string = string
and single_byte_character_representation = string
and double_byte_character_representation = string
and common_character_representation = string

(* B.1.2.3 *)

and time_literal = 
| Duration of duration
| Time_of_day of time_of_day
| Date of date
| Date_and_time of date_and_time

and tt = TIME | T | ST

and duration = (tt * _interval)
and _interval = 
| Days of days
| Hours of hours
| Minutes of minutes
| Seconds of seconds
| Milliseconds of milliseconds

and days = 
| DFixedpoint of fixed_point
| DInteger of integer * hours option

and fixed_point = string

and hours = 
| HF of fixed_point
| HI of integer * minutes option

and minutes =
| MF of fixed_point
| MI of integer * seconds option

and seconds = 
| SF of fixed_point
| SI of integer * milliseconds option

and milliseconds = 
| MIF of fixed_point
| MII of integer

(* B.1.2.3.2 *)
and tod = TIME_OF_DAY | TOD

and time_of_day = (tod * _daytime)

and _daytime = (day_hour * day_minute * day_second)
and day_hour = integer
and day_minute = integer
and day_second = fixed_point

and date = DATE | D | SD
and date_literal = (year * month * day)
and year = integer
and month = integer
and day = integer

and date_and_time = DATE_AND_TIME | DT

(* B.1.3 *)
and data_type_name = 
| NGTN of non_generic_type_name
| GTN of generic_type_name

and non_generic_type_name = non_generic * pointer_to option

and non_generic = 
| NG1 of _elementary_type_name
| NG2 of derived_type_name

(* B.1.3.1 *)


and integer_type_name = 
| SI of _signed_integer_type_name
| UI of _unsigned_integer_type_name

and real_type_name = 
| REAL 
| LREAL

and _signed_integer_type_name =
| SINT
| INT
| DINT
| LINT

and _unsigned_integer_type_name =
| USINT
| UINT
| UDINT
| ULINT

and enumerated_value = (enumerated_type_name option * _identifier * integer_literal option)

and structure_element_declaration_sec = 
| Initialized_structure of structure_type_name * structure_initialization
| Array_spec_init of subrange list * string_type * non_generic_type_name
| String_var_type of string_type * string_initialization option
| Simple_spec_init of _simple_specification * expression option * pointer_to option
| Subrange_spec_init of subrange_specification * expression option * pointer_to option
| Enumerated_spec_init of enumerated_specification * enumerated_value option * pointer_to option

and _simple_specification = 
| Elementary_type_name of _elementary_type_name
| Simple_type_name of simple_type_name

and structure_element_declaration = (structure_element_name * structure_element_declaration_sec)

and structure_element_initialization = 
| Constant of _constant
| Structure_element_name of _constant * enumerated_value * array_initialization * structure_initialization

and structure_initialization = 
| Structure_element_initialization of structure_element_initialization list

and _structure_declaration = 
| Structure_declaration of structure_element_declaration list

and _structure_specification = 
| Structure_specification of _structure_declaration * pointer_to option
| Initialized_structure of structure_type_name * structure_initialization

and _single_element_type_declaration = 
| Simple_type_declaration of simple_type_name * simple_spec_init
| Subrange_type_declaration of subrange_type_name * subrange_spec_init
| Enumerated_type_declaration of enumerated_type_name * enumerated_spec_init

and data_type_declaration = 
| Array_type_declaration of array_type_name * array_spec_init
| Structure_type_declaration of structure_type_name * _structure_specification
| String_type_declaration of string_type_name * string_type * string_initialization option
| Single_element_type_declaration of _single_element_type_declaration

and _library_element_declaration = 
| Data_type_declaration of data_type_declaration
| Function_declaration of function_declaration
| Function_block_declaration of function_block_declaration
| Program_Declaration of program_declaration
| Configuration_Declaration of configuration_declaration
| Global_Var_Declarations of global_var_declarations

and action_name = string

and ast = 
| Library_element_declaration of _library_element_declaration
| Action of action_name * function_block_body option * unknown_in_function option 
with sexp
