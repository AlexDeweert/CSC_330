(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

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

(*  *)
