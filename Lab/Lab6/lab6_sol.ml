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
   * scope
   * parameter passing
   * tail recursion

 *)

(* QUESTION 1. Static Scope *)
(* Consider the following program in the Algol-like pseudo-code
   used in the textbook, where f, g, and h are functions (each with
   one parameter) and the function body is inside braces {}.

{ begin
  int x = ...
  int y = ...
  int z = ...
  int f (int u)
      { int a = ...
        int y = ...
        int z = ...
        ...code for f...
      }
  int g (int u)
      { int a = ...
        int x = ...
        int w = ...
        int h (int v)
            { int a = ...
              int b = ...
              int z = ...
              ...code for h...
            }
        ...code for g...
      }
  ...code for main (outer) block...
}                

   Assume that static scope is used.
   (i) List all of the variables that are visible in the body of f,
       and indicate the place where they are declared.
   (ii) Do the same for function g.
   (iii) Do the same for function h.
   (iv) Translate the code above to OCaml giving any values you like
   to the variables.  In the code in the main block, call the
   functions f and g with parameters of your choice.  In the bodies of
   each function, insert print statements that print out the names of
   each variable that is visible along with its value.  *)

let x = 1 in
    let y = 2 in
    let z = 3 in
    let f (u:int) =
      let a = 40 in
      let y = 20 in
      let z = 30 in
      (print_string "Entering function f"; print_newline();
       print_string "Value of u: "; print_int u; print_newline();
       print_string "Value of a: "; print_int a; print_newline();
       print_string "Value of x: "; print_int x; print_newline();
       print_string "Value of y: "; print_int y; print_newline();
       print_string "Value of z: "; print_int z; print_newline()) in
    let g (u:int) =
      let a = 400 in
      let x = 100 in
      let w = 500 in
      let h (v:int) =
        let a = 4000 in
        let b = 6000 in
        let z = 3000 in
        (print_string "Entering function h"; print_newline();
         print_string "Value of u: "; print_int u; print_newline();
         print_string "Value of v: "; print_int v; print_newline();
         print_string "Value of w: "; print_int w; print_newline();
         print_string "Value of x: "; print_int x; print_newline();
         print_string "Value of y: "; print_int y; print_newline();
         print_string "Value of z: "; print_int z; print_newline();
         print_string "Value of a: "; print_int a; print_newline();
         print_string "Value of b: "; print_int b; print_newline()) in
      (h 7;
       print_string "Entering function g"; print_newline();
       print_string "Value of u: "; print_int u; print_newline();
       print_string "Value of w: "; print_int w; print_newline();
       print_string "Value of x: "; print_int x; print_newline();
       print_string "Value of y: "; print_int y; print_newline();
       print_string "Value of z: "; print_int z; print_newline();
       print_string "Value of a: "; print_int a; print_newline()) in
    (f 8; g 9)

(* 
Entering function f
Value of u: 8
Value of a: 40
Value of x: 1
Value of y: 20
Value of z: 30
Entering function h
Value of u: 9
Value of v: 7
Value of w: 500
Value of x: 100
Value of y: 2
Value of z: 3000
Value of a: 4000
Value of b: 6000
Entering function g
Value of u: 9
Value of w: 500
Value of x: 100
Value of y: 2
Value of z: 3
Value of a: 400
 *)

(* QUESTION 2. Dynamic Scope *)
(* Consider the following program (a modified version of the above
   code where the input to each function has type unit, i.e., there
   is no input).

{ begin
  int x = ...
  int y = ...
  int z = ...
  int f ()
      { int a = ...
        int y = ...
        int z = ...
        ...code for f...
      }
  int g ()
      { int a = ...
        int b = ...
        int z = ...
        ...code for g...
      }
  int h ()
      { int a = ...
        int x = ...
        int w = ...
        ...code for h...
      }
  ...code for main (outer) block...
}

   This time assume dynamic scope.  Given the following calling
   sequences, what variables are visible during execution of the
   last function call in each sequence?  Include with each
   visible variable the name of the block where it is declared
   (main, f, g, or h).  Draw the full activation stack.  In each
   activation record, include the local variables and the
   control link only.  (Note: for this exercise, you don't have
   to include the declarations of functions f, g, and h in the
   activation record for the main block.)

   (i) main calls f; f calls g; g calls h.
   (ii) main calls f; f calls h.
   (iii) main calls g; g calls h; h calls f. *)
    
(*
(i)
------
x |  |
------
y |  | (main)
------
z |  |
------

------
a |  |
------
y |  | (f)
------
z |  |
------

------
a |  |
------
b |  | (g)
------
z |  |
------

------
a |  |
------
x |  | (h)
------
w |  |
------

Visible variables:
a in block h
b in block g
w in block h
x in block h
y in block f
z in block g

(ii)
------
x |  |
------
y |  | (main)
------
z |  |
------

------
a |  |
------
y |  | (f)
------
z |  |
------

------
a |  |
------
x |  | (h)
------
w |  |
------

Visible variables:
a in block h
w in block h
x in block h
y in block f
z in block f

(iii)
Stack of Activation Records (showing only local variables)
------
x |  |
------
y |  | (main)
------
z |  |
------

------
a |  |
------
b |  | (g)
------
z |  |
------

------
a |  |
------
x |  | (h)
------
w |  |
------

------
a |  |
------
y |  | (f)
------
z |  |
------

Visible variables:
a in block f
b in block g
w in block h
x in block h
y in block f
z in block f
 *)
    
(* QUESTION 3. Parameter Passing *)
(* In "pass-by-value-result", also called "call-by-value-result" and
   "copy-in/copy-out", parameters are passed by value, with an added
   twist.  More specifically, suppose a function f with
   pass-by-value-result parameter u is called with actual parameter v.
   The activation record for f will contain a location for formal
   parameter u that is initialized to the R-value of v.  Within the
   body of f, the identifier u is treated as an assignable variable.
   On return from the call to f, the actual parameter v is assigned
   the R-value of u.

   The following pseudo-Alogo code illustrates the main properties of
   pass-by-value-result.

   var x : integer;
   x := 0;
   procedure p(value-result y : integer)
     begin
        y := 1;
        x := 0;
     end;
   p(x);

   With pass-by-value-result, the final value of x will be 1: Because
   y is given a new location distinct from x, the assignment to x does
   not change the local value of y.  When the function returns, the
   value of y is assigned to the actual parameter x.  If the parameter
   were passed by reference, then x and y would be aliases and the
   assignment to x would change the value of y to 0.  If the parameter
   were passed by value, the assignment to y in the body of p would
   not change the global variable x and the final value of x would
   also be 0.

   Translate the preceding program fragment into OCaml in a way that
   makes the operations on locations, and the differences between
   L-values and R-values, explicit.  Uncomment the code below and fill
   in your solution.

let x = ref 0
let p (y' : int ref) : unit =
 ...
let result = p x
let _ = x

 *)

let x = ref 0
let p (y' : int ref) : unit =
  let y = ref !y' in
  (y := 1;
   x := 0;
   y' := !y) (* for pass-by-value-result *)
let result = p x
let _ = x

(* In contrast, here is the code for pass-by-reference *)
let x = ref 0
let p (y : int ref) : unit =
  (y := 1;
   x := 0)
let result = p x
let _ = x

(* and for pass-by-value (whose results are the same as
   pass-by-reference for this example) *)
let x = ref 0
let p (z : int) : unit =
  let y = ref z in
  (y := 1;
   x := 0)
let result = p !x
let _ = x

(* QUESTION 4. Tail Recursion *)
(* Below are two versions of the factorial function, where the first
   function ("fact") is not tail recursive and the second function
   ("fact_tr") is tail recursive.  Note that "fact_tr" uses an
   auxiliary function with an extra argument to keep track of the
   computation so far.

   Using the same style, transform the function "power" below into a
   tail recursive one.  This function raises the input "a" to the
   power "b" by repeated multipication.  Use an auxiliary function
   that has a third parameter to represent the result of the
   computation so far.  *)

let rec fact (n:int) =
  if n = 0 then 1 else n*fact(n-1)

let fact_tr (n:int) =
  let rec fact' (n:int) (a:int) =
    if n = 0 then a else fact' (n-1) (n*a) in
  fact' n 1

let rec power (a:int) (b:int) =
  if b = 0 then 1
  else if b = 1 then a
  else a*power a (b-1)

let power_tr (a:int) (b:int) =
  let rec power' (a:int) (b:int) (c:int) =
    if b = 0 then 1
    else if b = 1 then c*a
    else power' a (b-1) (c*a) in
  power' a b 1
