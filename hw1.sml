(*  Assignment #1 *)

type DATE = (int * int * int)
val month_strings = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]
exception InvalidParameter
val test_date = (2012,2,28)
(* This file is where your solutions go *)

(* utility functions *)
fun year(d: DATE): int =
    #1 d;
fun month(d: DATE): int =
    #2 d;
fun day(d: DATE): int =
    #3 d;


(* IS OLDER *)
fun is_older(d1: DATE, d2: DATE): bool =
    if year(d1) = year(d2) then
        if month(d1) = month(d2) then
            if day(d1) = day(d2) then false
            else if day(d1) < day(d2) then true
            else false
        else if month(d1) < month(d2) then true
        else false
    else if year(d1) < year(d2) then
        true
    else
        false

(* Add your other functions here *)

(* NUMBER IN MONTH *)
fun number_in_month( date_list : DATE list, month : int ): int =
    if null date_list then 0
    else if #2 ( hd date_list ) = month then 1 + number_in_month( tl date_list, month )
    else 0 + number_in_month( tl date_list, month );

(* NUMBER IN MONTHS *)
fun number_in_months( date_list : DATE list, month_list : int list ): int =
    if null month_list then 0
    else number_in_month( date_list, hd month_list ) + number_in_months( date_list, tl month_list );

(* DATES IN MONTH *)
fun dates_in_month( date_list : DATE list, month : int ): DATE list =
    if null date_list then []
    else if #2 ( hd date_list ) = month
        then (hd date_list) :: dates_in_month( tl date_list, month )
    else dates_in_month( tl date_list, month );

(* DATES IN MONTHS *)
fun dates_in_months( date_list : DATE list, month_list : int list ): DATE list =
    if null month_list then []
    else dates_in_month( date_list, hd month_list ) @ dates_in_months( date_list, tl month_list );

(* GET NTH TODO Check to see if InvalidParamter is being raised properly*)
fun get_nth( string_list : string list, nth : int ): string =
    if nth < 1 then raise InvalidParameter
    else if nth > length string_list then raise InvalidParameter
    else if nth = 1 then hd string_list
    else get_nth( tl string_list, nth-1 );

(* DATE TO STRING *)
fun date_to_string( date : DATE ): string =
    get_nth( month_strings, (#2 date) ) ^ " " ^ Int.toString( #3 date ) ^ ", " ^ Int.toString( #1 date );

(*Utility Function - SUM INTS*)
fun sum_ints( int_list : int list ): int =
    if null int_list then 0
    else (hd int_list) + sum_ints( (tl int_list) );
(*
val intas = [1,2,3];
val sumintas = sum_ints(intas) = 6; *)

(* NUMBER BEFORE REACHING SUM*)
fun number_before_reaching_sum( sum : int, int_list : int list ): int =
    if null int_list then 0
    else if sum - hd int_list > 0 then 1 + number_before_reaching_sum( sum - hd int_list, tl int_list )
    else 0

(*WHAT MONTH*)
fun what_month( num_days : int ): int =
    number_before_reaching_sum( num_days, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1;

(*MONTH RANGE*)
fun month_range( day1 : int, day2 : int ): int list =
    if day1 > day2 then []
    else if day1 = day2 then [ what_month( day1 ) ]
    else what_month( day1 ) :: month_range( day1+1, day2 );

(*OLDEST*)
fun oldest( date_list : DATE list ): =
    if null date_list then NONE
    else SOME test_date;
