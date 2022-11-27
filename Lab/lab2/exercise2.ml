https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
(* 

  Agenda:
   * tuples and lists
   * options
   * higher order functions

  Note that questions 2, 8, and 9 are optional.

*)

(* An employee is a tuple of name, age, and boolean indicating marriage status *)
type employee = string * int * bool
                                 
(* 1. Write a function that takes an employee, and prints out the information in some readable form. *)

(*
let print_employee_info t = 
*) 

(* 2. Reimplement the OCaml standard functions List.length and List.rev
   for lists of strings.
   This question is optional, but is good practice for the next one. *)

(*
let length (l:string list) : int = 

let rev (l:string list) : string list = 
*)

(* 3. Remove the kth element from a list. Assume indexing from 0 *)
(* example: rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] 
 * results in: [["to" ; "be" ; "not" ; "to" ; "be"] *)
(* let rmk (k:int) (l:string list) : string list =  *)


(* 4. Write a function that returns the final element of an integer list,
   if it exists, and None otherwise *)
(*
let final (l: int list) : int option = 
*)

(* 5. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. Do 
 * the same for the larger of two int options.*)

(*
let min_option (x: int option) (y: int option) : int option = 
*)

(*
let max_option (x: int option) (y: int option) : int option = 
*)

(* 6. Write a function that returns the integer buried in the argument
 * or None otherwise *)  
(* 
let get_option (x: int option option option option) : int option = 
*)

(* 7. Write a function to return the boolean AND/OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)

(*
let and_option (x:bool option) (y: bool option) : bool option = 
*)

(*
let or_option (x:bool option) (y: bool option) : bool option = 
*)

(* What's the pattern? How can we factor out similar code? *)

(**************)
(* Note: Questions 8 and 9 are optional.  We have not yet covered all
   of this material in class, but you still may be able to do them,
   especially if you have read ahead in the class notes. *)
(**************)
                                 
(* 8. Optional:
 * Write a higher-order function for binary operations on options.
 * If both arguments are present, then apply the operation.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
(* What is the type of the calc_option function? *)

(*
let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
*)

(* 9. Optional:
 * Now rewrite the following functions using the above higher-order function
 * Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other.
 * Do the same for the larger of two int options. *)

(*
let min_option2 (x: int option) (y: int option) : int option = 
*)

(*
let max_option2 (x: int option) (y: int option) : int option = 
*)
