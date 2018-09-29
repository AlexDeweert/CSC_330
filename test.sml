(* Messing around with sml *)

fun truefun n = true

(* Must call with truefun 1 2 3 *)
(* val truefun = fn : int -> int -> int *)
fun truefun x y z = x + y + z

(* Must call with a tuple *)
(* val a1 = fn : int * int * int -> int *)
fun a1( a, b, c ) =
    a + b + c

(* val triple = fn : int -> int *)
fun triple x =
    3*x

(* val increment = fn : int -> int *)
fun increment x =
    x + 1

(* val tuple = (fn,int,9) : (int -> int) * (int -> int) * int *)
val tuple = ( triple, increment, triple(3) );

val a = ( #1 tuple ) 3
val b = ( #2 tuple ) 1
val c = ( #3 tuple )

(* naive fib *)

fun naive_fib n =
    let
        fun aux( n' : int ) =
            if n' < 2 then 0
            else  aux ( n'- 1 ) + aux (n' - 2)
    in
        aux( n )
    end;

val x = naive_fib 4;

(* TODO memo fib *)
(*fun memo_fib n =
    let
        fun aux( n' : int, mem : int list ) =
            (* Use mem table to find previously computed values *)
            if n' < 2 then mem
            else
                if n' > length mem then mem@[aux( n' - 2,mem )]@[aux( n' - 1,mem )]
                else mem@[ #(n'-2) mem ]@[ #(n'-1) mem ]
    in
        aux( 3, [0,1,1] )
    end;*)

(* fold *)
fun fold (f,acc,xs) =
    case xs of
      [] => acc
    | x::xs' => fold( f, f(acc,x), xs' );

val ans = fold( ( fn (x,y) => x+y ), [], [[1],[2],[3]] );
