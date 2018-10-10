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
