type 'a btree =
    |E                              (*arbre vide*)
    |N of 'a * 'a btree * 'a btree  (*noeud*)

type 'a gtree =
    |GN of 'a * 'a gtree list

(*R.3-1*)
let est_videb (ab : 'a btree): bool =
    match ab with
        |E -> true
        |_ -> false
(*R.3-2*)
let hautb (ab : 'a btree): int =
    let rec haux (ab : 'a btree) (acc : int): int =
        match ab with
            |E -> acc
            |N(_, g, d) -> max (haux g (acc + 1)) (haux d (acc + 1))
    in haux ab (-1)
(*R.3-3*)
(*R.3-6*)
(*R.3-7*)
(*R.3-8*)
(*R.3-15*)
(*R.3-18*)
(*R.3-24*)