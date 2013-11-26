module List = Batteries.List
module Hashtbl = Batteries.Hashtbl
module MyString = Batteries.String

module SS = Sexplib.Sexp
module SSL = Sexplib.Std

module PL = PropositionalLogic
open TableauBuchiAutomataGeneration

let (|>) x f = f x

let usage_msg = "Usage: stc <filename>\nsee -help for more options" in

let file_name = ref "" in
let speclist = Arg.align [] in

try
  let () = Arg.parse speclist (fun x -> file_name := x) usage_msg in
  (* Initialize the error reporting structures *)
  let in_chan = open_in !file_name in
  let () = print_endline "....Lexing and parsing..." in
  let lexbuf = Lexing.from_channel in_chan in
  let ast = Parser.ast Lexer.lexer lexbuf in
  (* Close the input channel *)
  let () = close_in in_chan in 
with
| End_of_file -> exit 0
| Sys_error  _ -> Arg.usage speclist usage_msg
| _ as s -> raise s
