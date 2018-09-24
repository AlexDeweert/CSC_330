(* hw2.sml
*  CSC 330 Prof. D. German
*  Fall 2018
*  University of Victoria
*  Student: Alex L. Deweert
*  ID: V00855667
*)

(*Default bindings*)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw
exception IllegalMove
exception notFound

(**Part 1**)

(* Curried function: fun foo a b = a + b;
Foo is a function that takes an integer and returns a function
that takes an integer and returns an integer
ie int -> ( int -> int ). Since *)

(*1. all_except_option*)
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

(*2. get_substitutions1*)
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

(*3. get_substitutions2*)
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

(*4. similar_names*)
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
            make_similar( subs, [name] )
        end
    end;

(*5. card_color*)
fun card_color( x : card ) =
    let
        fun getsuit( x' : card ) =
            case x' of (suit,rank) => suit

        fun getcolor( suit' : suit ) =
            case suit' of Clubs => Black
            | Spades => Black
            | Hearts => Red
            | Diamonds => Red
    in
        getcolor(getsuit(x))
    end;

(*6 card_value*)
fun card_value( x : card ) =
    let
        fun getrank( r : rank ) =
            case r of Jack => 10 | Queen => 10 | King => 10
            | Ace => 11
            | Num(i) => i
    in
        case x of (suit,rank) => getrank(rank)
    end;

(*7. remove_card *)
fun remove_card( cs : card list, c : card, e : exn ) =
    let
        fun iter( cs' : card list, acc : card list ) =
            case cs' of [] => acc
            | a::[] => if  a = c then acc else iter( [], acc@[a] )
            | a::b::[] => if a = c then acc@[b] else iter( [b], acc@[a] )
            | a::b::rest => if a = c then acc@b::rest else iter( b::rest, acc@[a])
    in
        let
            val result = iter( cs, [] )
        in
            if result = cs then raise e
            else result
        end
    end

(*8. all_same_color*)
fun all_same_color( cs : card list ) =
    let
        fun helper( cs' : card list, prev : card, acc : bool ) =
            case cs' of [] => acc
            | a::[] => if( acc andalso card_color(a) = card_color(prev))
                       then helper( [], a, true )
                       else helper( [], a, false )
            | a::b::rest => if (acc andalso card_color(a) = card_color(b))
                            then helper( b::rest, a, true )
                            else helper( b::rest,a, false)
    in
        let
            fun result( cs' : card list ) =
                case cs' of [] => true
                | a::[] => true
                | a::b::rest => if helper( b::rest, a, true ) then true else false
        in
            if result(cs) then true else false
        end
    end;

(*9. sum_cards *)
fun sum_cards( xs : card list ) =
    let
        fun aux( xs' : card list, acc : int ) =
            case xs' of [] => acc
            | a::[] => aux( [], card_value(a) + acc )
            | a::b => aux( b, card_value(a) + acc )
    in
        aux( xs, 0 )
    end;

(*10. score*)
fun score( hand : card list, goal : int ) =
    let
        val sum = sum_cards(hand)
        fun preliminary_score( sum' : int ) =
            if sum' > goal then 2*(sum'-goal) else (goal-sum')
        fun final_score( prelim_score : int ) =
            if not(all_same_color(hand)) then prelim_score else prelim_score div 2
    in
        final_score( preliminary_score( sum ) )
    end;

(*11. officiate*)
fun officiate( cards : card list, moves : move list, goal : int ) =
    let
        fun run(  moves' : move list, cards' : card list, acc_hand : card list ) =
            case moves' of [] => score(acc_hand, goal)
            | one_move::[] => (*We have one move*)
                (case one_move of Draw => (*The move type is draw*)
                    (case cards' of [] => (*...but card list empty: game over*) score( acc_hand, goal )
                    | b::[] => (*have at least one card in card list*) if sum_cards(acc_hand@[b]) > goal then score( acc_hand@[b], goal ) else run( [], [], acc_hand@[b] )
                    | b::c::rest => (*have at least two cards in card list*) if sum_cards(acc_hand@[b]) > goal then score( acc_hand@[b], goal ) else run( [], c::rest, acc_hand@[b] ))
                | Discard(c) => (*The move type is Discard*)
                    (case acc_hand of [] => (*...but our hand is empty*) raise IllegalMove
                    | b::[] => (*At least one card in hand*) run( [], cards', remove_card( acc_hand, c, IllegalMove )  )
                    | b::d::rest => (*At least two cards in hand*) run( [], cards', remove_card( acc_hand, c, IllegalMove )  )))
            | first_mv::next_mv::rest_mv => (*We have at least 2 moves*)
                (case first_mv of Draw => (*The move type is draw*)
                    (case cards' of [] => (*...but card list empty: game over*) score( acc_hand, goal )
                    | b::[] => (*have at least one card in card list*) if sum_cards(acc_hand@[b]) > goal then score( acc_hand@[b], goal ) else run( next_mv::rest_mv, [], acc_hand@[b] )
                    | b::c::rest => (*have at least two cards in card list*) if sum_cards(acc_hand@[b]) > goal then score( acc_hand@[b], goal ) else run( next_mv::rest_mv, c::rest, acc_hand@[b] ))
                | Discard(c) => (*The move type is Discard*)
                    (case acc_hand of [] => (*...but our hand is empty*) raise IllegalMove
                    | b::[] => (*At least one card in hand*) run( next_mv::rest_mv, cards', remove_card( acc_hand, c, IllegalMove )  )
                    | b::d::rest => (*At least two cards in hand*) run( next_mv::rest_mv, cards', remove_card( acc_hand, c, IllegalMove )  )))
    in
        run( moves, cards, [] )
    end;

(*TESTING*)
fun toInt (b: bool) =
  if b then 1 else 0;

fun sum(a:int, b:int) =
  a + b;

fun tests_passed(t: string, tests: bool list) =
  let
    val len = length tests
    val count = foldl sum 0 (map toInt tests)
  in
    print ("\n**Test " ^ t ^ " "^ (if count = len then "passed" else "failed")
           ^ "\n  "^ Int.toString(count) ^ " out of " ^ Int.toString(len) ^ "\n\n");
    count = len
  end;

val test1_1=all_except_option("3",["4","9","10"]) = NONE;
val test1_2=all_except_option("3",["4","9","3","10"]) = SOME ["4","9","10"];
val test1_3=all_except_option("3",[]) = NONE;
val test1_4=all_except_option("3",["3","4","9","10"])  = SOME ["4","9","10"];
val test1_5=all_except_option("3",["4","9","10","3"]) = SOME ["4","9","10"];
val test1_6=all_except_option("3",["3"]) = SOME [];
val tests1 = [test1_1, test1_2, test1_3, test1_4, test1_5, test1_6];

val t1 = tests_passed("1", tests1);
val test2_1=get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                               "Fred")
            = ["Fredrick","Freddie","F"];
val test2_2=get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                               "Jeff")
            = ["Jeffrey","Geoff","Jeffrey"];
val test2_3=get_substitutions1([["Neo","New"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Tank")
            = ["Panzer","Sherman","Container"];
val test2_4=get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Neo")
            = ["New","Nuovo"]

val tests2 = [test2_1, test2_2, test2_3, test2_4];

val t2 = tests_passed("2", tests2);

val test3_1=get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                               "Fred")
            = ["Fredrick","Freddie","F"];
val test3_2=get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                               "Jeff")
            = ["Jeffrey","Geoff","Jeffrey"];

    val test3_3=get_substitutions2([["Neo","New"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Tank")
            = ["Panzer","Sherman","Container"];
val test3_4=get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Neo")
            = ["New","Nuovo"];

val tests3 = [test3_1, test3_2, test3_3, test3_4];

val t3 = tests_passed("3", tests3);

val test4_1=similar_names([
                             ["Thomas", "Neo"],
                             ["Batman", "Hulk","Bruce"],
                             ["Spiderman", "Peter"]
                         ], {first="Bruce", middle = "(whoknows)", last="Wayne"}) =
            [{first="Bruce",last="Wayne",middle="(whoknows)"},
             {first="Batman",last="Wayne",middle="(whoknows)"},
             {first="Hulk",last="Wayne",middle="(whoknows)"}];

val test4_2=similar_names([
                             ["Fred","Fredrick"],
                             ["Elizabeth","Betty"],
                             ["Freddie","Fred","F"]
                         ], {first="Fred", middle="W", last="Smith"}) =
            [{first="Fred",last="Smith",middle="W"},
             {first="Fredrick",last="Smith",middle="W"},
             {first="Freddie",last="Smith",middle="W"},
             { first = "F", last = "Smith", middle = "W" }]

val tests4 = [test4_1, test4_2];

val t4 = tests_passed("4", tests4);

val ClubAce = (Clubs,Ace);
val DiamondsJack = (Diamonds,Jack);
val Hearts10 = (Hearts, Num 10);
val Spades5 = (Spades,Num 5);

val test5_1= card_color(ClubAce) = Black;
val test5_2= card_color(DiamondsJack) = Red;
val test5_3= card_color(Hearts10) = Red;
val test5_4= card_color(Spades5) = Black;

val tests5 = [test5_1, test5_2, test5_3, test5_4];

val t5 = tests_passed("5", tests5);

val test6_1= card_value(ClubAce) = 11;
val test6_2= card_value(DiamondsJack) = 10;
val test6_3= card_value(Hearts10) = 10;
val test6_4= card_value(Spades5) = 5;
val test6_5= card_value(Spades, Queen) = 10;
val test6_6= card_value(Spades, King) = 10;

val tests6 = [test6_1, test6_2, test6_3, test6_4];
val t6 = tests_passed("6", tests6);


exception notFound

val cards1 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)];
val cards2 = [];
val cards3 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 5), (Clubs, Num 9)];
val cards4 = [(Clubs, Ace), (Clubs, Num 10), (Clubs, Num 5), (Clubs, Num 2)];
val cards5 = [(Diamonds, Ace), (Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)];

val test7_1 = remove_card(cards3, (Clubs, Ace), notFound) = [(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)];
val test7_2 = remove_card(cards1, (Spades, Num 4), notFound) = [(Clubs, Ace), (Diamonds, Num 10), (Clubs, Num 4)];
val test7_3 = remove_card(cards3, (Clubs, Num 9), notFound) = [(Clubs,Ace),(Diamonds,Num 10),(Spades,Num 5)];
val test7_4 = remove_card(cards5, (Diamonds, Ace), notFound) = [(Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)];
(* check that exception is raised*)
val test7_5 = remove_card(cards2, (Clubs, Ace), notFound) = [(Clubs, Ace)] handle notFound => true;

val tests7 = [test7_1, test7_2, test7_3, test7_4,test7_5];
val t7 = tests_passed("7", tests7);

val test8_1 = all_same_color(cards1) = false;
val test8_2 = all_same_color(cards2) = true;
val test8_3 = all_same_color(cards3) = false;
val test8_4 = all_same_color(cards5) = true;
val test8_5 = all_same_color(cards5) = true;

val tests8 = [test8_1, test8_2, test8_3, test8_4,test8_5];
val t8 = tests_passed("8", tests8);

val test9_1 = sum_cards(cards1) = 29;
val test9_2 = sum_cards(cards2) = 0;
val test9_3 = sum_cards(cards3) = 35;
val test9_4 = sum_cards(cards4) = 28;
val test9_5 = sum_cards(cards5) = 51;

val tests9 = [test9_1, test9_2, test9_3, test9_4,test9_5];
val t9 = tests_passed("9", tests9);

val test10_1 = score(cards1, 1) = 28 * 2;
val test10_2 = score(cards2, 28) = 14; (* empty list is conssidered same color *)
val test10_3 = score(cards3, 35) = 0;
val test10_4 = score(cards4, 28) = 0;
val test10_5 = score([(Spades, Num 2)], 28) = 13;
val test10_6 = score([(Diamonds, Ace), (Diamonds, Num 10)],20) = 1;

val tests10 = [test10_1, test10_2, test10_3, test10_4, test10_5, test10_6];
val t10 = tests_passed("10", tests10);

val test11_1 = officiate(cards3, [], 10) = 5;
val test11_2 = officiate(cards3, [Draw], 10) = 1;
val test11_3 = officiate(cards3, [Draw], 5) = 6;
val test11_4 = officiate(cards5, [Draw, Draw], 0) = 11;
val test11_5 = officiate(cards3, [Draw, Draw], 15) = 12;
val test11_6 = officiate(cards3, [Draw, Draw, Draw], 15) = 12;
val test11_7 = officiate(cards3, [Draw, Draw, Draw, Draw], 35) = 0;
val test11_8 = officiate(cards3, [Draw, Draw, Draw, Discard (Spades, Num 5)], 15) = 12;
val test11_9 = officiate(cards5, [Draw, Draw, Draw, Discard (Spades, Num 2)], 45) = 10 handle IllegalMove => true;
val test11_10 = officiate(cards2, [Draw], 10) = 5;
val test11_11 = officiate(cards2, [Discard (Spades, Ace)], 10) = 5 handle IllegalMove => true;
val test11_12 = officiate(cards3, [Draw, Discard (Spades, Num 7)], 10) = 1;

val test11_13 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=11;
val test11_14 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0;
val test11_15 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=4;


val tests11 = [test11_1, test11_2, test11_3, test11_4, test11_5,
               test11_6, test11_7, test11_8, test11_9, test11_10,
               test11_11, test11_12, test11_13, test11_14, test11_15];

val t11 = tests_passed("11", tests11);

val total = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11];

val totalPassed = tests_passed("**Overall", tests11);
