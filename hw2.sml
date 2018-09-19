(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)


(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(****)

(* Messing Around with custom datatypes *)
(*
datatype mytype = 
    TwoInts of int*int
    | Str of string
    | Pizza

fun f x =
    case x of
      Pizza => 3
    | Str s => 8
    | TwoInts(i1,i2) => i1+i2

datatype othertype =
    MyStr of int

fun g x =
    case x of
        MyStr s => 3
*)

fun helper( e : string * (string list) ) =
    case e of

        (*One element*)
        (str, [a]) => (
            if same_string(str,a) then []
            else [a]
        ) 

        (*At least two elements*)
      | (str, head::(neck::rest)) => (
            helper(str,[head])@helper(str,neck::rest)
            (*if same_string(str,head) then []@helper(str,neck::rest) 
            else [head]@helper(str,neck::rest)*)
        );

fun all_except_option ( e : string * (string list) ) =
    case e of
      (str,[]) => (
        NONE
      )
    | (str, [a]) => (
        if helper(str,[a]) = [] then SOME []
        else if [a] = helper( str,[a] ) then NONE else SOME [a]
      )
    | (str, head::(neck::rest)) => (
        if head::(neck::rest) = helper(str, (head::(neck::rest))) then NONE else SOME ( helper( str, (head::(neck::rest))) )
      );

      (*| (str, head::(neck::rest)) => SOME ( all_except_option(str,[head])@all_except_option(str,(neck::rest)) );*)
(*| (str, (c::d)) => SOME ( [c]@["999"] ); (*At least two strings*)
(*A list with AT LEAST one element, puts the head at the end of the tail
  If the tail is null, then it puts null list as head which has no ill effects*)
| (str, c::d) => SOME ( d@[c] );
*)

all_except_option( "9", ["3","4","5","6"] );
