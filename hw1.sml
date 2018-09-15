(* Assignment #1 *)
(* CSC 330 University of Victoria *)
(* Fall 2018                      *)
(* Professor Daniel GERMAN        *)
(* Student: Alex L. DEWEERT       *)
(* Id: V00855767                  *)

type DATE = (int * int * int)
exception InvalidParameter

(* IS OLDER *)
fun is_older(d1: DATE, d2: DATE): bool =
    if #1 d1 = #1 d2 then
        if #2 d1 = #2 d2 then
            if #3 d1 = #3 d2 then false
            else if #3 d1 < #3 d2 then true
            else false
        else if #2 d1 < #2 d2 then true
        else false
    else if #1 d1 < #1 d2 then
        true
    else
        false;

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

(* GET NTH *)
fun get_nth( string_list : string list, nth : int ): string =
    if nth < 1 then raise InvalidParameter
    else if nth > length string_list then raise InvalidParameter
    else if nth = 1 then hd string_list
    else get_nth( tl string_list, nth-1 );

(* DATE TO STRING *)
fun date_to_string( date : DATE ): string =
    let
        val month_strings = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]
    in
        get_nth( month_strings, (#2 date) ) ^ " " ^ Int.toString( #3 date ) ^ ", " ^ Int.toString( #1 date )
    end;

(* NUMBER BEFORE REACHING SUM*)
fun number_before_reaching_sum( sum : int, int_list : int list ): int =
    if null int_list then 0
    else if sum - hd int_list > 0 then 1 + number_before_reaching_sum( sum - hd int_list, tl int_list )
    else 0;

(*WHAT MONTH*)
fun what_month( num_days : int ): int =
    number_before_reaching_sum( num_days, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1;

(*MONTH RANGE*)
fun month_range( day1 : int, day2 : int ): int list =
    if day1 > day2 then []
    else if day1 = day2 then [ what_month( day1 ) ]
    else what_month( day1 ) :: month_range( day1+1, day2 );

(*OLDEST*)
fun oldest( date_list : DATE list ) =
    if null date_list then NONE
    else let fun oldest_nonempty( date_list : DATE list ) =
        if null (tl date_list) then hd date_list
        else
            let val tl_ans = oldest_nonempty( tl date_list )
            in
                if is_older( hd date_list, tl_ans ) then hd date_list
                else tl_ans
            end
         in
            SOME (oldest_nonempty date_list)
         end;

(*REASONABLE DATE*)
fun reasonable_date( date : DATE ): bool =
    (*If year >= 1*)
    if #1 date >= 1 andalso #3 date > 0 then
        (*Is leap year AND if month is 2, ensure day <= 29*)
        if #2 date = 2 andalso ( #1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso not( #1 date mod 100 = 0)) ) then
            if #3 date <= 29 then true
            else false
        (*Is not a leap year but the month is 2*)
        else if #2 date = 2 then
            if #3 date <= 28 then true
            else false
        (*If non-feb months are 31 day months*)
        else if #2 date = 1 orelse #2 date = 3 orelse #2 date = 5 orelse #2 date = 7 orelse #2 date = 8 orelse #2 date = 10 orelse #2 date = 12 then
            if #3 date <= 31 then true
            else false
        (*If non-feb months are 30 day months*)
        else if #2 date = 4 orelse #2 date = 6 orelse #2 date = 9 orelse #2 date = 11 then
            if #3 date <= 30 then true
            else false
        else false
    else false;
