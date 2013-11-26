(*

Author: Avinash Malik

Purpose: Error reporting and assoicated things

Date: Sun Jun  3 17:17:17 IST 2012

*)

let get_line_and_column = function
  | (x,y) -> ((string_of_int x) ^ "," ^ (string_of_int y) ^ ": ")
