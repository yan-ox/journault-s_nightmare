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
let tailleg (ag : 'a gtree): int =
    let rec taux (ag : 'a gtree) (acc : int) : int =
        match ag with
            |GN(_, lag) -> listaux lag (acc + 1)
    and listaux (lag : 'a gtree list) (acc : int): int =
        match lag with
            |ag :: tlag -> listaux tlag (taux ag (acc))
            |_  -> acc
    in taux ag 0
(*R.3-6*)
let rec est_present (ab : 'a btree) (x : 'a): bool =
    match ab with
        |E -> false
        |N(a, g, d) -> if a = x then true else est_present g x || est_present d x
(*R.3-7*)
let rec minmax (ab : 'a btree): (int*int) option =
    match ab with
        |N(x, g, d) -> begin
            let g2, d2 = minmax g, minmax d in 
            match g2, d2 with
            |Some (mi, ma), Some (mi1, ma1) -> let mi2, ma2 = min mi mi1, max ma ma1 in Some (min mi2 x, max ma2 x)
            |Some (mi, ma), None -> Some (min mi x, max ma x)
            |None, Some (mi, ma) -> Some (min mi x, max ma x)
            |None, None -> Some(x, x)
        end
        |E -> None
(*R.3-8*)
let minmax2 (ab : 'a btree): (int*int) option =
    let rec minmaux (ab : 'a btree) (mini : int) (maxi : int) : int*int =
        match ab with
            |N(x, g, d) -> let (mi1, ma1), (mi2, ma2) = minmaux g (min mini x) (max x maxi), minmaux d (min mini x) (max x maxi) in (min mi2 mi1, max ma2 ma1 )
            |_ -> mini, maxi
    in 
    match ab with
        |N(x, g, d) ->let (mi1, ma1), (mi2, ma2) = minmaux g x x, minmaux d x x in Some(min mi2 mi1, max ma2 ma1 )
        |E -> None
(*R.3-15*)
(*R.3-18*)
(*R.3-24*)

let ag = GN(1, [GN(2, [GN(4, [])]); GN(3, [])])
let ab = N(1, N(2, N(4, E, E), E), N(3, E, E))
let () = assert(tailleg ag = 4)
let () = assert(est_present ab 3)
let () = assert(minmax ab = Some(1, 4))
let () = assert(minmax2 ab = Some(1, 4))