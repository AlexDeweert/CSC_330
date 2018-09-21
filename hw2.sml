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

(* Curried function: fun foo a b = a + b;
Foo is a function that takes an integer and returns a function 
that takes an integer and returns an integer
ie int -> ( int -> int ). Since *)

(*all_except_option*)
fun all_except_option( str : string, xs : string list ) =
    let 
        fun aux( xs': string list, acc : string list ) =
            case xs' of [] => acc
            | a::[] => if same_string(a,str) then aux([], acc) else aux( [], acc@[a] )
            | a::b::[] => if same_string(a,str) then aux( [b], acc ) else aux( [b], acc@[a] )
            | a::b::c => if same_string(a,str) then aux( b::c, acc ) else aux( b::c, acc@[a] )
    in
        let val result = aux( xs, [] ) 
        in
            case result of [] => if xs = [] then NONE else SOME []
            | a::b => if (a::b) = xs then NONE else SOME result
        end
    end;

val test1_1 = all_except_option( "9", ["11", "9"] ) = SOME ["11"];
val test1_2 = all_except_option( "9", ["9"] ) = SOME [];
val test1_3 = all_except_option( "9", ["11", "8"] ) = NONE;
val test1_4 = all_except_option( "9", [] ) = NONE;

(*get_substitutions1
TODO make this non-tail recursive
TODO make tests for this after its non-tail recursive*)
fun get_substitutions1( xss : string list list, str : string ) =
    let
        fun aux( xss' : string list list, acc : string list ) =
            let
                fun auxaux( head : string list ) =
                    let val list_result = all_except_option( str, head )
                    in
                        case list_result of NONE => []
                        | SOME [] => []
                        | SOME a => a
                    end
            in
                case xss' of [] => acc
                | a::[] => aux( [], acc@auxaux(a) )
                | a::b::[] => aux( [b], acc@auxaux(a) )
                | a::b::c => aux( b::c, acc@auxaux(a) )
            end
    in
        aux( xss, [] )
    end;

get_substitutions1( [ ["ryker","mitzy", "bob"],[ "stinny","murmee"],["bob","alex","ashley"] ], "bob" );
get_substitutions1( [ ["Fred","Frederick"],[ "Elizabeth","Betty"],["Freddy","Fred","F"] ], "Fred" );

(*get_substitutions2
TODO make tests for this*)
fun get_substitutions2( xss : string list list, str : string ) =
    let
        fun aux( xss' : string list list, acc : string list ) =
            let
                fun auxaux( head : string list ) =
                    let val list_result = all_except_option( str, head )
                    in
                        case list_result of NONE => []
                        | SOME [] => []
                        | SOME a => a
                    end
            in
                case xss' of [] => acc
                | a::[] => aux( [], acc@auxaux(a) )
                | a::b::[] => aux( [b], acc@auxaux(a) )
                | a::b::c => aux( b::c, acc@auxaux(a) )
            end
    in
        aux( xss, [] )
    end;
(*

(*Gets a str and string list, returns all except the str*)
fun helper2( e : string * (string list) ) =
    case e of
        (*One element*)
        (str, [a]) => (
            if same_string(str,a) then []
            else [a]
        ) 
        (*At least two elements*)
      | (str, head::(neck::rest)) => (
            helper(str,[head])@helper(str,neck::rest)
        );

fun lists_equivalent( e : (string list) * (string list) ) =
    case e of
          ([],[]) => ( true )
        | ( [a],[b] ) => ( if same_string(a,b) then true else false )
        | ( a::(b::c), d::(e::f) ) => ( if same_string(a,d) then lists_equivalent( b::c, e::f ) else false );
*)

(*1.2 get_substitutions1*)
(*TODO rename this to get_substitutions1*)
(*Need to check if the output list from helper2 = the original input list
if it does, then we don't want to include the result int the final output
of get_subs1
After a recursive call we could go through and check if the returned list
is equivalent to the orig input, then return null if it is
*)

(*
fun get_substitutions1 ( e : string list list * string ) =
    
    let
        fun listcheck( a : string list, str : string ): string list =
            let val option_removed_list = helper2(str,a) in
            if lists_equivalent( option_removed_list, a ) then [] else option_removed_list end
    in
        case e of
            (*Exactly one list*)
            ( a::[], str) => helper2(str,a)

            (*At least two lists and...*)
            (*...at least one element in both lists*)
            (*Note here, a is a list, c is a list*)
          | ( a::(b::c), str ) => ( listcheck(a, str)@get_substitutions1(b::c, str) )
    end;
*)

(*get_substitutions1( [ ["joe","zoey","ryker"], ["murphy","ryker","stinky"], ["a","ryker","c"] ], "ryker" );*)

(*get_substitutions1( [ ["Fred","Fredrick"], ["Elizabeth","Betty"], ["Freddie","Fred","F"] ], "Fred");*)
