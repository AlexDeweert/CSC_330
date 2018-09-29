(* hw3.sml
 * CSC 330 UVIC Fall 2018
 * Professor D.M. German
 * Student: Alex L. Deweert
 * ID: V00855767
 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:

*)

(*fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end*)


(**** put all your code after this line ****)

(* In this assignment:
*  Higher order functions
*  Filter, map, foldl
*  Currying
*)

(* 1. only_capitals *)
fun only_capitals( xs : string list ) =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

val test1_1 = only_capitals( [ "These", "Have", "Capitals", "this", "doesnt" ] ) = [ "These","Have","Capitals" ];
val test1_2 = only_capitals( [] ) = [];

(* 2. longest_string1 *)
(* Foldl takes one parameter, an anon function. 
   The anon function takes two parameters,
   a single element (same type as a list element)
   and an (inititally empty) accumulator of the same
   type as one of the list elements.
   Foldl will perform the function on each element in
   the list, and it will return the accumulated value.
*)
fun longest_string1( xs : string list ) =
    let
        val aux = fn(x,acc) => if size(x) > size(acc) then x 
                               else acc
    in
        case xs of [] => ""
        | a => List.foldl aux "" a
    end;

val test2_1 = longest_string1( ["aaaaa","bbbbb","cc"] ) = "aaaaa";
val test2_2 = longest_string1( ["aaa","bbbb","cc"] ) = "bbbb";
val test2_3 = longest_string1( ["aaa","bbb","cccc"] ) = "cccc";
val test2_4 = longest_string1( [] ) = "";
