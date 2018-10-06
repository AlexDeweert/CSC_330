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

    g takes three arguments, two functions (f1 and f2) and a pattern datatype (p).

    The function parameter f1 allows an optional count (depending on the behavior of the 
    function passed into g for f1) for any Wildcard patterns should a Wildcard appear in the pattern p.

    The function parameter f2 allows an optional count (depending on the specified behavior of the 
    function passed into g for f2) for any Variable patterns should a variable appear in the pattern p.
    The difference between f1 and f2 is that f2 can determine if the x in Variable meets some criteria
    before a count value is returned by f2.

    g computes an option Wildcard count, plus a an optional, customized, Variable count.
*)

fun g f1 f2 p =
    let
	    val r = g f1 f2
    in
	    case p of
	        Wildcard          => f1 ()
	      | Variable x        => f2 x
	      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	      | ConstructorP(_,p) => r p
	      | _                 => 0
    end

(* 1. only_capitals *)
fun only_capitals( xs : string list ) =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

val test1_1 = only_capitals( [ "These", "Have", "Capitals", "this", "doesnt" ] ) = [ "These","Have","Capitals" ];
val test1_2 = only_capitals( [] ) = [];

(* Foldl takes one parameter, an anon function. 
   The anon function takes two parameters,
   a single element (same type as a list element)
   and an (inititally empty) accumulator of the same
   type as one of the list elements.
   Foldl will perform the function on each element in
   the list, and it will return the accumulated value.
*)

(* 2. longest_string1 *)
fun longest_string1( xs : string list ) =
    let
        val aux = fn(x,acc) => if size(x) > size(acc) then x 
                               else acc
    in
        case xs of [] => ""
        | a => List.foldl aux "" a
    end;

val test2_1 = longest_string1 ["aaaaa","bbbbb","cc"] = "aaaaa";
val test2_2 = longest_string1 ["aaa","bbbb","cc"] = "bbbb";
val test2_3 = longest_string1 ["aaa","bbb","cccc"] = "cccc";
val test2_4 = longest_string1 [] = "";

(* 3. longest_string2 *)
fun longest_string2( xs : string list ) =
    let
        val aux = fn(x,acc) => if size(x) > size(acc) then x
                               else if size(x) = size(acc) then x
                               else acc
    in
        case xs of [] => ""
        | a => List.foldl aux "" a
    end;

val test3_1 = longest_string2 ["aaaaa","bbb","ccccc"] = "ccccc";
val test3_2 = longest_string2 ["aaa","bbbb","cc"] = "bbbb";
val test3_3 = longest_string2 ["aaa","bbb","cccc"] = "cccc";
val test3_4 = longest_string2 []  = "";

(* 4.1 longest_string3 *)
fun longest_string3( xs : string list ) =
    let
        val aux = fn(x,acc) => if size(x) > size(acc) then x 
                               else acc
    in
        case xs of [] => ""
        | a => List.foldl aux "" a
    end;

(* 4.2 longest_string4 *)
fun longest_string4( xs : string list ) =
    let
        val aux = fn(x,acc) => if size(x) > size(acc) then x
                               else if size(x) = size(acc) then x
                               else acc
    in
        case xs of [] => ""
        | a => List.foldl aux "" a
    end;

(* SIMPLE working example1*)
(*fun curry f = fn x => f(x);
val foo = fn(z) => if z > 5 then true else false;
val foo2 = fn(z) => if z <= 5 then true else false;*)
(* Simple working example2 *)
(*fun add x y = x + y;
val result = add 3; (*returns a partially evaluted function, 3 + y*)
val resultresult = result 5; (* uses the results of fn(y) => 3+y, will result in 8*)*)

(* 4. longest_string_helper *)
(* Dont need to handle empty list case with foldl *)
fun longest_string_helper f xs =
    List.foldl ( fn(x,acc) => if f(size(x),size(acc)) then x else acc ) "" xs;

(* 4.1 longest_string3 *)
val longest_string3 = longest_string_helper ( fn(a,b) => if a > b then true else false );
val test4_1 = longest_string3 ["aaaaa","bbbbb","cc"] = "aaaaa";
val test4_2 = longest_string3 ["aaa","bbbb","cc"] = "bbbb";
val test4_3 = longest_string3 ["aaa","bbb","cccc"] = "cccc";
val test4_4 = longest_string3 [] = "";

(* 4.2 longest_string4 *)
val longest_string4 = longest_string_helper( fn(a,b) => if a >= b then true else  false );
val test4_5 = longest_string4 ["aaaaa","bbb","ccccc"] = "ccccc";
val test4_6 = longest_string4 ["aaa","bbbb","cc"] = "bbbb";
val test4_7 = longest_string4 ["aaa","bbb","cccc"] = "cccc";
val test4_8 = longest_string4 [] = "";

(* 5. longest_capitalized *)
fun longest_capitalized( xs : string list ) =
    let
        val composition = longest_string2 o only_capitals
    in
        composition xs
    end;

val test5_1 = longest_capitalized( ["This","Cannot","be", "true"] ) = "Cannot";

(*6. rev_string *)
val rev_string = String.implode o rev o String.explode;

val test6_1 = rev_string( "reverse_me" ) = "em_esrever";
val test6_2 = rev_string( "abba" ) = "abba";
val test6_3 = rev_string( "" ) = "";

(* 7. first_answer *)
(* The type of the acc (the init val) in foldl will dictate what type of value 
that the function parameter f will return. We know that xs is a list because
foldl only works on lists *)
fun first_answer f xs =
    case xs of [] => raise NoAnswer
    | [x] => ( case f(x) of (SOME v) => v | NONE => first_answer f [] )
    | x::rest => ( case f(x) of (SOME v) => v | NONE => first_answer f rest )

(* 8. all_answers *)
fun all_answers f xs =
    let
        fun aux( xs', acc ) =
            case xs' of [] => SOME acc
            | x::[] => aux( [], acc@( ( first_answer f [x] ) ) )
            | x::rest => aux( rest, acc@( (first_answer f [x] )  ) )
    in
        aux( xs, [] )
    end
    handle NoAnswer => NONE;

(* 9_b. count_wildcards *)
fun count_wildcards p =
    g ( fn() => 1 ) ( fn(x) => 0 ) p;

val test9_b_1 = count_wildcards(Wildcard) = 1;
val test9_b_2 = count_wildcards(UnitP) = 0;
val test9_b_3 = count_wildcards(ConstP 1) = 0;
val test9_b_4 = count_wildcards(TupleP []) = 0;
val test9_b_5 = count_wildcards(TupleP [Wildcard]) = 1;
val test9_b_6 = count_wildcards(TupleP [Wildcard, UnitP, Variable "x"]) = 1;
val test9_b_7 = count_wildcards(TupleP [Wildcard, Wildcard, Variable "xy"]) = 2;
val test9_b_8 = count_wildcards(TupleP [ TupleP [Wildcard, Wildcard]]) = 2;
val test9_b_9 = count_wildcards(TupleP [ ConstructorP("a", Wildcard), TupleP[Wildcard]]) = 2;

(* 9_c. count_wild_and_variable_lengths *)
fun count_wild_and_variable_lengths p =
    g ( fn() => 1 ) ( fn(x) => String.size(x) ) p;

val test9_c_1 = count_wild_and_variable_lengths(Wildcard) = 1;
val test9_c_2 = count_wild_and_variable_lengths(Wildcard) = 1;
val test9_c_3 = count_wild_and_variable_lengths(UnitP) = 0;
val test9_c_4 = count_wild_and_variable_lengths(ConstP 1) = 0;
val test9_c_5 = count_wild_and_variable_lengths(TupleP []) =  0;
val test9_c_6 = count_wild_and_variable_lengths(TupleP [Wildcard]) = 1;
val test9_c_7 = count_wild_and_variable_lengths(TupleP [Wildcard, UnitP, Variable "abc"]) = 4;
val test9_c_8 = count_wild_and_variable_lengths(TupleP [Wildcard, Wildcard, Variable "xy"]) = 4;
val test9_c_9 = count_wild_and_variable_lengths(TupleP [TupleP [Wildcard, Wildcard, Variable "This is the sea"]]) = 17;

(* 9_d. count_some_pair *)
fun count_some_var(str, p) =
    g ( fn() => 0 ) ( fn(x) => if x = str then 1 else 0 ) p;

val test9_d_1 = count_some_var("test", TupleP [TupleP [Wildcard, UnitP, Variable "test",Variable "test"]]) = 2;
val test9_d_2 = count_some_var("test", TupleP [TupleP [Wildcard, UnitP,Variable "te"]]) = 0;
val test9_d_3 = count_some_var("test", TupleP [TupleP [Wildcard, UnitP, Variable "test"]]) = 1;
