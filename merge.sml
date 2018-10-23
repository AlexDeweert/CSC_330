fun reverse xs =
    let
        fun aux(xs,acc) =
            case xs of [] => acc | x::xs' => aux( xs', x::acc)
    in aux(xs, []) end;

fun foldright f acc xs =
    let 
        val reverselist = reverse xs
        fun aux( xs, acc ) =
            case xs of [] => acc
            | x::xs' => aux( xs', f(x,acc) )
    in
        aux( reverselist, [] )
    end;

fun mergesort xs =
    (*foldl ( fn (x,acc) => [x]::acc ) [] xs;*)
    foldright ( fn(x,acc) => [x]::acc ) [] xs;

val result = mergesort [2,4,1,5];

val size = split [2,3,1,5,8,9,6];
