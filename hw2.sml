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

(*similar_names*)

(*type type_fullname = { first: string, middle: string, last: string }*)
(*type fullname = {first: string, middle: string, last: string};*)

(*pattern match a tuple*)
fun first( a, _,  _ ) = a;
fun second( _, b, _ ) = b;
fun third( _, _, c ) = c;

(*LESSON: PATTERN MATCHING on records can be achieved; ie in getfirst helper*)
fun similar_names ( xss : string list list, name : {first:string,middle:string,last:string} ) =
    let
        fun getfirst( x : {first:string,middle:string,last:string} ) =
            case x of {first=e1,middle=e2,last=e3} => e1

        fun getmiddle( x : {first:string,middle:string,last:string} ) =
            case x of {first=e1,middle=e2,last=e3} => e2
        
        fun getlast( x : {first:string,middle:string,last:string} ) =
            case x of {first=e1,middle=e2,last=e3} => e3

        fun getsubs( xss' : string list list, first' : string  ) =
            get_substitutions2( xss', first' )

        fun make_similar( subs' : string list, acc : {first:string,middle:string,last:string} list ) =
            case subs' of [] => acc
            | a::[] => make_similar( [], acc@[{ first=(a), middle=getmiddle(name), last=getlast(name) }] )
            | a::b => make_similar( b, acc@[{ first=(a), middle=getmiddle(name), last=getlast(name) }] )
    in
        (*
            We get a list of lists, which have first names
            we also get a full name

            we want to use the FIRST part of the full name to call getsubstitutions2( getfirst(name) )
            
            With that, we get back a list of names where the FIRST of the FULLNAME was present
            within each element in the list-list.

            For all of those elements returned by getsubs2, we replace the FIRST part of FULLNAME
            and append that to a growing accumulator (since we're returning a list of FULLNAMES)
            We ALSO want to return the original FULLNAME.
        *)
        let val subs = getsubs( xss, getfirst(name) )
        in
            (*[getfirst( name )]*)
            make_similar( subs, [name] )
        end
    end;

similar_names( [ ["john","bob","conway"],["dog","ed"],["fred","holmes"] ], {first="bob",middle="j",last="odenkirk"} );

(*

type mytype = { first:int, mid:int, last:int };

fun f( x: mytype ) =
    #first x;

f( {first=1, mid=2, last=3} );

(*results in val it = 2 : int*)

*)
