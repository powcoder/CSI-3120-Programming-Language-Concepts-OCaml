https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
(* A condensed version of the signature in range.mli.  Your first step is to study the contents of range.mli. *)
module type RANGE =
sig
  type t
  type e
  val singleton : e -> t
  val range : e -> e -> t
  val sadd : t -> e -> t
  val smult : t -> e -> t
  val bridge : t -> t -> t
  val size : t -> int
  val contains : t -> e -> bool
  val rless : t -> t -> bool option
end

(* An implementation of the RANGE datatype with int as range type and
   pairs representing a range *)
  module LoHiPairRange : RANGE with type e = int =
struct
  type e = int
  type t = e * e
  let singleton (i:e) : t = (i,i)
  let range (i:e) (j:e) : t = ((min i j), (max i j))
  let sadd (x:t) (i:e) : t = let (lo,hi) = x in (lo+i,hi+i)
  let smult (x:t) (i:e) : t =
    let (lo, hi) = x in
    if i >= 0 then (lo*i,hi*i)
    else (hi*i,lo*i)
  let bridge (x:t) (y:t) : t =
    let (lx, hx) = x in
    let (ly, hy) = y in
    ((min lx ly), (max hx hy))
  let size (x:t) : int =
    let (lo,hi) = x in
    hi - lo - (-1)
  let contains (x:t) (i:e) : bool =
    let (lo,hi) = x in
    (lo <= i) && (i <= hi)
  let rless (x:t) (y:t) : bool option =
    let (lx, hx) = x in
    let (ly, hy) = y in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end

(* Exercise 1: Complete the new implementation of RANGE in the
     ListRange module below.  The part that is already implemented
     should give you enough information to implement the rest.  Add
     some test code to test your implementation. *)
    
(* An implementation of the RANGE datatype with int as range type and
   lists representing a range *)
module ListRange : RANGE with type e = int =
struct
  type e = int
  type t = e list

  (* auxiliary functions *)
  let minmax (l:t) : (e*e) option =
      let rec max (t:t) (e:e) : e =
          match t with
          | [] -> e
          | h::r -> max r h
      in
      match l with
      | [] -> None
      | h::r -> Some (h, (max r h))
  let rec build (i:e) (j:e) : e list =
    if i = j then [j]
    else i :: build (i+1) j
  
  let singleton (i:e) : t = [i]
  let range (i:e) (j:e) : t = build (min i j) (max i j)
  let sadd (x:t) (i:e) : t = List.map (fun y->y+i) x
  let smult (x:t) (i:e) : t =
    match (minmax x) with
    | None -> []
    | Some (xl,xh)  ->
      if i < 0 then build (xh*i) (xl*i)
      else  build (xl*i) (xh*i)
  let bridge (x:t) (y:t) : t =    
    match (minmax x), (minmax y) with
    | None, None -> []
    | None, Some _ -> y
    | Some _, None -> x
    | Some (xl,xh), Some (yl,yh) ->
      build (min xl yl) (max xh yh)
  let size (x:t) : int = List.length x
  let contains (x:t) (i:e) : bool = List.mem i x
  let rless (x:t) (y:t) : bool option =
    match (minmax x), (minmax y) with
    | None, _ -> None
    | _ , None -> None
    | Some (lx,hx), Some(ly,hy) ->
      if hx < ly then Some true
      else if hy < lx then Some false
      else None
end

(* Add some test code to test your new implementation. *)
let r1 = ListRange.range 2 6
let r2 = ListRange.smult r1 3
let s1 = ListRange.size r1
let s2 = ListRange.size r2
(* etc... *)

(* Exercise 2: Design an imperative version of RANGE.  Do so by
   copying range.mli here and changing the types as necessary.  And
   then copy the implementation of LoHiPairRange and modify the code
   as necessary.  All the operations should remain the same as in the
   functional version.  The singleton and range operations should each
   create a new range.  The sadd and smult operations should modify
   existing ranges. Consider the design choices and design your own
   version of bridge. *)

module type IMP_RANGE =
sig
  (* types *)
  (* RANGE type *)
  type t
  (* element type *)
  type e
    
  (* construct a one-item range *)
  val singleton : e -> t
  (* construct a range with two endpoints, inclusive *)
  val range : e -> e -> t

  (* modifiers *)
  (* scalar add range, e.g. if r is a range from -4 through 6, 
     sadd r 1 modifies it to become a range from  -3 through 7. 
     This operation does not change the size of a range. *)
  val sadd : t -> e -> unit
  (* scalar multiply range, e.g. if r is a range from 2 through 4,
     smult r 3 modified it to become a range from 6 through 12. 
     This operation may change the size of a range. *)                        
  val smult : t -> e -> unit
  (* modifies the first argument so that it is a range that spans both
     given ranges, e.g.  if given a range from -4 through 6 and a
     range from 10 through 12, the first range is changed to become a
     range from -4 through 12. *)
  val bridge : t -> t -> unit

  (* observers *)
  (* how many elements are in the range? *)
  val size : t -> int
  (* does t contain e? *)
  val contains : t -> e -> bool
  (* is an arbitrary element of the first range 
      less than an arbitrary element of the second range?
     if the ranges overlap, return None, because 
      answers differ depending on the element chosen
     otherwise return whether the first range's max < second range's min
   *)
  val rless : t -> t -> bool option
      
end
           
(* An imperative version of LoHiPairRange *)
module LoHiPairRangeImp : IMP_RANGE with type e = int =
struct
  type e = int
  type t = (e * e) ref
  let singleton (i:e) : t = ref (i,i)
  let range (i:e) (j:e) : t = ref ((min i j), (max i j))
  let sadd (x:t) (i:e) : unit = let (lo,hi) = !x in x:=(lo+i,hi+i)
  let smult (x:t) (i:e) : unit =
    let (lo, hi) = !x in
    if i >= 0 then x:=(lo*i,hi*i)
    else x:=(hi*i,lo*i)
  let bridge (x:t) (y:t) : unit =
    let (lx, hx) = !x in
    let (ly, hy) = !y in
    x:=((min lx ly), (max hx hy))
  let size (x:t) : int =
    let (lo,hi) = !x in
    hi - lo - (-1)
  let contains (x:t) (i:e) : bool =
    let (lo,hi) = !x in
    (lo <= i) && (i <= hi)
  let rless (x:t) (y:t) : bool option =
    let (lx, hx) = !x in
    let (ly, hy) = !y in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end

let r = LoHiPairRangeImp.range 2 6
let c1 = LoHiPairRangeImp.contains r 2
let _ = LoHiPairRangeImp.smult r 3
let c2 = LoHiPairRangeImp.contains r 2
