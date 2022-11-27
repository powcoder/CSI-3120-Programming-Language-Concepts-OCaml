https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
(* Pipelines in OCaml *)
(* In this lab, you will use pipelining to calculate and display final
   marks for students in a course. *)

(* The type "marks" is a tuple of 6 floating point numbers.  The first
   3 are marks for 3 assignments.  The next 2 are marks for two term
   tests.  The last one is the mark for the final exam. *)
type marks = float * float * float * float * float * float

(* The type "mark_triple" is a tuple of 3 floating point numbers.  The
   first is the assignment mark for the course.  The second is the
   term test mark for the course.  The third is the mark for the final
   exam. *)
type mark_triple = float * float * float

(* The type "final_grade" is the student's final mark represented as a
   percentage. *)
type final_grade = float

(* The type "letter_grade" represents the student's final mark as it
   will appear on their transcript (A+, A, ...) *)
type letter_grade = string

(* The calculations will involve a student_id and 3 kinds of tuples
   representing 3 different forms of student records. *)
type student_id = int
type st_record1 = student_id * marks
type st_record2 = student_id * mark_triple
type st_record3 = student_id * final_grade * letter_grade

(* Assignment 1 is worth a total of 60 marks, Assignment 2 is worth
   75, and Assignment 3 is worth 40.  Each term test is worth 50 and
   the final exam is marked out of 100. *)
let total_a1 : float = 60.
let total_a2 : float = 75.
let total_a3 : float = 40.
let total_t1 : float = 50.
let total_t2 : float = 50.
let total_exam : float = 100.
let perfect_score : marks = (total_a1,total_a2,total_a3,total_t1,total_t2,total_exam)

(* The marking scheme for the course is that the assignments are worth
   33%, the term tests are worth 33% and the final exam is worth 34%. *)
let assign_percent1 = 33.
let test_percent1 = 33.
let exam_percent1 = 34.

(* The following function may be useful for tranforming a mark to a
   percentage. *)
let out_of_100 (max_marks:float) (actual_marks:float) : float =
  (actual_marks *. 100.) /. max_marks

(* QUESTION 1.  Define an OCaml function that takes a st_record1, and
   uses a pipeline to do the following operations:

(a) First modify the exam component of each student record.  In the
   input record, each student's mark is out of 100 points.  The
   professor has decided to mark it out of 95 points.  So, if a
   student got 95 on the exam, their mark will be converted to 100.
   If the student got 96, their mark will be converted to 101.05.  If
   the student got 94, their mark will be converted to 98.95, etc.

(b) Next, transform each st_record1 to a st_record2 by calculating
   the total number of marks the student got on the assignment portion
   of the course, the term test component, and the final exam
   component.

(c) Next, modify each of the 3 mark components of st_record2 by
   transforming them to a percentage.

(d) Next, modify each one again by transforming it to the appropriate
   portion allowed by the marking scheme.  For example, if the student
   got 100% on the assignment portion of the course, the 100 in the
   assignment position of the tuple of type student_record2 should be
   replaced by 33, because the assignment portion of the course is
   worth 33% of the total mark.  If the student got 50% on the
   assignment portion, this value should be replaced by half of 33,
   which is 16.5, etc.

(e) Transform the st_record2 that is obtained from step (d) to a
   st_record3, by summing the 3 mark components of the st_record2 and
   using the result to calculate the letter grade using the University
   of Ottawa grading scheme. *)

(* Sample Solution to 1(a) *)
let update_exam (sr:st_record1) : st_record1 =
  let (s_id,(a1,a2,a3,t1,t2,e)) = sr in
  let new_e = out_of_100 95. e in
  (s_id,(a1,a2,a3,t1,t2,new_e))

(* Sample Solution to 1(b) *)
let assign_score (sr:st_record1) : float =
  let (_,(a1,a2,a3,_,_,_)) = sr in a1+.a2+.a3

let test_score (sr:st_record1) : float =
  let (_,(_,_,_,t1,t2,_)) = sr in t1+.t2

let exam_score (sr:st_record1) : float =
  let (_,(_,_,_,_,_,e)) = sr in e

let total_scores (sr:st_record1) : st_record2 =
  let (s_id,_) = sr in
  let a = assign_score sr in
  let t = test_score sr in
  let e = exam_score sr in
  (s_id,(a,t,e))

(* Sample Solution to 1(c) *)
let percentages (sr:st_record2) : st_record2 =
  let (s_id,(a_mark,t_mark,e_mark)) = sr in
  let a_percent = out_of_100 (total_a1+.total_a2+.total_a3) a_mark in
  (s_id,(a_percent,t_mark,e_mark))

(* Sample Solution to 1(d) *)
let raw_scores (sr:st_record2) : st_record2 =
  let (s_id,(a_mark,t_mark,e_mark)) = sr in
  let a_raw = (a_mark *. assign_percent1) /. 100. in
  let t_raw = (t_mark *. test_percent1) /. 100. in
  let e_raw = (e_mark *. exam_percent1) /. 100. in
  (s_id,(a_raw,t_raw,e_raw))

(* Sample Solution to 1(e).  Note that this solution is only partial.
   The final letter grade calculation is incomplete. *)
let compute_final (sr:st_record2) : st_record3 =
  let (s_id,(a_raw,t_raw,e_raw)) = sr in
  let final_percent = a_raw +. t_raw +. e_raw in
  let final_letter = "TBD" in
  (s_id,final_percent,final_letter)

(* Solution to Question 1 *)
let compute_mark x =
  x |> update_exam
    |> total_scores
    |> percentages
    |> raw_scores
    |> compute_final

(* Some test data *)
let example1 = (100000,(55.,74.,38.,45.,40.,75.))
let example2 = (100001,(45.,73.,30.,38.,37.,80.))
let examples = [example1;example2]

let _ = compute_mark example1
let _ = compute_mark example2

(* QUESTION 2 *)
(* Define a version of the "display" function on page 18 of the course
   notes on pipelines that works with the data in this lab.  The type
   of the input argument to your version of "display" will be
   "st_record1 list", and you will use your solution to Question 1
   instead of "compute_score".  You will also need to define a new
   version of "compare_score" and "stringify". *)

let student_compare (_,score1,_) (_,score2,_) =
  if score1 < score2 then 1
  else if score1 > score2 then -1
  else 0

let stringify (s_id, final_percent, final_letter) = 
  string_of_int s_id ^ ": " ^ string_of_float final_percent ^ ", " ^ final_letter


let display (students : st_record1 list) =
  students |> List.map compute_mark
           |> List.sort student_compare
           |> List.map stringify
           |> List.iter print_endline
          
let _ = display examples
