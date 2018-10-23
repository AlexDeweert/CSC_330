datatype tree = EmptyT
    | Tree of (int*tree*tree);

(* Inserts values into a binary tree *)
fun insert( root, newv ) =
        case root of EmptyT => Tree(newv,EmptyT,EmptyT)
        
        | Tree(rootv, EmptyT, EmptyT) => if rootv >= newv 
                                       then Tree(rootv, Tree(newv,EmptyT,EmptyT), EmptyT)
                                       else Tree(rootv, EmptyT, Tree(newv,EmptyT,EmptyT))
        
        | Tree(rootv, t1, EmptyT) => if rootv >= newv 
                                       then Tree(rootv, insert(t1,newv), EmptyT)
                                       else Tree(rootv, t1, Tree(newv,EmptyT,EmptyT))

        | Tree(rootv, EmptyT, t1) => if rootv >= newv 
                                       then Tree(rootv, Tree(newv,EmptyT,EmptyT), t1)
                                       else Tree(rootv, EmptyT, insert(t1,newv))
        
        | Tree(rootv, t1, t2) => if rootv >= newv 
                                       then Tree(rootv, insert(t1,newv), t2)
                                       else Tree(rootv, t1, insert(t2,newv));

val test1 = insert( EmptyT, 5 ) = Tree(5, EmptyT, EmptyT);
val test2 = insert(insert(EmptyT, 3),5) = Tree(3, EmptyT, Tree(5,EmptyT,EmptyT));
val test3 = insert(insert(EmptyT, 3),1) = Tree(3, Tree(1,EmptyT,EmptyT), EmptyT);
val test3 = insert(insert(insert(EmptyT,3),2),5) = Tree(3,Tree(2,EmptyT,EmptyT),Tree(5,EmptyT,EmptyT));
val test4 = insert(insert(insert(EmptyT,1),3),2) = Tree(1, EmptyT, Tree(3,Tree(2,EmptyT,EmptyT),EmptyT));

(* Produces an in_order list of binary tree values *)
fun in_order t acc =
    case t of EmptyT => acc
    | Tree(value, EmptyT, EmptyT) => (in_order EmptyT acc@[value])
    | Tree(value, t1, t2) => (in_order t1 acc)@[value]@(in_order t2 acc)

val test5 = in_order (Tree(5,EmptyT,EmptyT)) [];
val test6 = in_order (Tree(5,Tree(4,EmptyT,EmptyT),EmptyT)) [];
val test7 = in_order (Tree(1,Tree(0,EmptyT,EmptyT),Tree(5,Tree(4,EmptyT,EmptyT),Tree(8,EmptyT,EmptyT)))) [];
val test8 = in_order (insert(insert(insert(EmptyT,1),3),2)) [];

(* Folds an in order tree value list *)
fun fold_tree f acc t =
    let
        val aux = in_order
    in
        foldl f acc (aux t [])
    end;

(* to_string *)
fun to_string(acc:string, i:int): string =
    if acc = "" then Int.toString(i) else acc ^ " " ^ Int.toString(i);

(* tree to string *)
val tree_to_string = fold_tree ( fn(x,acc) => to_string(acc,x) ) "";

val test5 = fold_tree (fn(x,acc) => x+acc) 0 (Tree(5,Tree(4,EmptyT,EmptyT),EmptyT));
val test6 = tree_to_string (insert(insert(insert(EmptyT,3),2),5));

(*val test5 = in_order (fn(x,acc) => x+acc) (Tree(5,EmptyT,EmptyT)) 0;*)
(*val test6 = in_order (Tree(5,Tree(4,EmptyT,EmptyT),EmptyT)) [];
val test7 = in_order (Tree(1,Tree(0,EmptyT,EmptyT),Tree(5,Tree(4,EmptyT,EmptyT),Tree(8,EmptyT,EmptyT)))) [];
val test8 = in_order (insert(insert(insert(EmptyT,1),3),2)) [];*)


(* reverse, pure, linear *)
fun rev xs =
    let fun aux(xs', acc) =
        case xs' of [] => acc
        | x::[] => aux([], x::acc)
        | x::rest => aux(rest, x::acc)
    in
        aux(xs, [])
    end;

val test10 = rev [1,2,3];

(* reverse, pure, non linear*)
fun rev2 xs =
    let fun aux(xs', acc) =
        case xs' of [] => acc
        | x::rest => aux(rest, [x]@acc)
    in
        aux(xs, [])
    end;

val test11 = rev2 [1,2,3];

fun split xs =
    let
        fun aux( i, xs, acc1, acc2) =
            case xs of [] => (acc1,acc2)
            | x::xs' => if i mod 2 = 0 then aux( (i+1), xs', x::acc1, acc2)
                                   else aux( (i+1), xs', acc1, x::acc2 )
    in
        aux( 0, xs, [], [] )
    end;

val test12 = split [1,2,3,4,5];

fun map (f,xs) =
    case xs of [] => []
    | x::xs' => (f x)::map(f,xs');

(* listify *)
fun listify xs =
    let
        val f = fn x => [x]
    in
        map(f,xs)
    end;

val test13 = listify [1,2,3];

(* Signatures, Structures, and Implementation Hiding*)
signature TEST_SIG =
sig
    val fact : int->int
    val half_pi: real
end;

structure TestLib :> TEST_SIG =
struct
    fun fact a = 3
    val half_pi = 1.0
end;

val c = TestLib.fact 2;

val xx = { name = "Alex", id = 855767 };

fun getid x =
    case x of { id, name } => name;

val n = getid xx;

datatype my_int_list = Empty | Cons of int * my_int_list;

val cons = Cons(3, Cons(0,Empty));

fun append_to_list (xs, ys) =
    case xs of Empty => ys
    | Cons(x, xs') => Cons(x, append_to_list( xs', ys));

val to_append = Cons(4,Empty);
val resultant_list = append_to_list( to_append, cons) = Cons(4,Cons(3,Cons(0,Empty)));

fun blah ()=
    print "hello";

blah ();


(* simple fold example *)
val lst = [1,1,1];
val result = foldl ( fn(x,acc) => x+acc ) 2 lst;

(* partial application with a functin that only takes a tupled parameter *)
fun range(a,b) =
    if a > b then []
    else a::range(a+1, b);

fun curry f x y = f(x,y);
val partial = curry range;
val completion = partial 1 5; (*Results in [1,2,3,4,5]*)

fun quadfib n =
    if n < 2 then 1
    else quadfib(n-1) + quadfib(n-2);

fun linfib n =
    let
        fun aux( n, acc1, acc2 ) =
            if n < 2 then acc2
            else aux( n-1, acc2, acc1+acc2) 
    in
        aux( n, 1, 1)
    end;

val test9 = quadfib(20);
val test9_1 = linfib(20);
