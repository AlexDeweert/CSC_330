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

fun is_older(d1: DATE, d2: DATE): bool =
    (*years equal*)
    if year(d1) = year(d2) then
        (*months equal*)
        if month(d1) = month(d2) then
            (*days equal*)
            if day(d1) = day(d2) then false

            (*day1 < day2*)
            else if day(d1) < day(d2) then true

            (*day1 > day2*)
            else false

        (*month1 < month2*) (*return true*)
        else if month(d1) < month(d2) then true

        (*month1 > month2*) (*return false*)
        else false

    (*year1 < year2*)
    else if year(d1) < year(d2) then
        (*return true*)
        true

    (*year1 > year2*)
    else
        (*return false*)
        false

(* Add your other functions here *)
