(* 1a *)
fun f x y z =
    x (y) + z;

val y = 3;

fun g z =
    let 
        val x = fn x => x * 2
    in
        f z
    end;

val h = g (fn a => a*a);

val ans = h 5 2;

(* 1b *)
fun f x =
    case x of [] => 0
    | (a,b)::[] => a + b
    | (a,b)::(c,d)::e => a + d + f(e);

val ans = f ( map (fn x => (1,x)) [2,3,4] );

(* 1c *)
val x = 7;
fun g y = x*y;
fun f z =
    let
        val x = 3
    in
        g(z) + x
    end;

(* 2a *)
fun addAllOpt xs =
    let
        fun aux( xs', acc) =
            case xs' of [] => acc
            | x::[] => aux( [], acc+unwrap(x) )
            | x::xs'' => aux( xs'', acc+unwrap(x) )
        and unwrap x =
            case x of SOME i => i
            | NONE => 0
    in
        let val result = aux( xs, 0)
        in
            case result of 0 => NONE
            | i => SOME i
        end
    end;

val a = addAllOpt( [SOME 1, NONE, SOME 3] );
val a = addAllOpt( [] );

datatype tree = EmptyT
    | Tree of (int*tree*tree);

(* insert tree *)
fun insert( tree, i ) =
        case tree of EmptyT => Tree(i,EmptyT, EmptyT)
        | Tree(j, Tree(m,n,u), EmptyT) => Tree(j, insert( Tree(m,n,u), m), EmptyT )
        | Tree(j, EmptyT, Tree(m,n,u)) => Tree(j, EmptyT, insert( Tree(m,n,u), m))
        | Tree(j, Tree(m,n,u), Tree(q,r,s)) => Tree(j,insert( Tree(m,n,u), m), insert( Tree(q,r,s), q) )
        | Tree(j, EmptyT, EmptyT) => Tree(j,EmptyT,EmptyT);
        
val a = insert(EmptyT, 5);
val b = insert(insert(EmptyT, 1),3);
