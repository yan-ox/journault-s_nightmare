(*R. 1-1*)
let rec der_el (l: 'a list) : 'a = 
  match l with 
  |[x] -> x
  |x :: t -> der_el t
  |[] -> raise(Invalid_argument("ta liste est vide"))

(*R. 1-2*)  
let rec der_el2 (l: 'a list) : 'a option =
  match l with
  |[x] -> Some(x)
  |_ :: l2 -> der_el2 l2
  |_ -> None

(*R. 1-3*)
let miroir (l : 'a list): 'a list =
  let rec miraux (l: 'a list) (lf: 'a list): 'a list =
    match l with
    |x :: l2 -> miraux l2 (x :: lf)
    |[] -> lf
  in miraux l []

(*R. 1-4*)  
let miroir2 (l : 'a list): 'a list = 
  let addup (l2 : 'a list) (x : 'a) : 'a list = x :: l2 
  in List.fold_left (addup) [] l

(*R. 1-5*)
let rec conc (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
  |x :: tl -> x :: (conc tl l2)
  |[] -> l2 
  
(*R. 1-6*)
let rec conc2 (l1 : 'a list) (l2 : 'a list) : 'a list =
  let addup (l3 : 'a list) (x : 'a) : 'a list = x :: l3
  in List.fold_left (addup) (l2) (miroir l1)
  
(*R. 1-7*)
let rec thanos_snap (l : 'a list) (x : 'a) : 'a list =
  match l with
  |a :: tl -> if a = x then thanos_snap tl x else a :: thanos_snap tl x
  |[] -> []

(*R. 1-8*)
let coupe (l : 'a list) (i : int): 'a list * 'a list = 
  let rec caux (l1 : 'a list) (l2 : 'a list) (i: int): 'a list * 'a list =
    if i <= 0 then l1, List.rev l2
    else match l1 with
    |x :: tl -> caux tl (x :: l2) (i - 1)
    |_ -> l1, List.rev l2
  in caux l [] i

(*R. 1-9*)
let rec coupe2 (l : 'a list) (i : int): 'a list * 'a list =
  if i <= 0 then l, []
    else match l with
    |x :: tl -> let l1, l2 = coupe2 tl (i - 1) in l1, x :: l2
    |_ -> l, []

(*R. 1-11*)
let applat (l: 'a list list): 'a list =
  let rec applaux (l1 : 'a list list) (lp : 'a list): 'a list =
    match l1 with
      |x :: tl -> applaux tl ((List.rev x) @ lp)
      |[] -> lp
  in List.rev (applaux l [])

(*R. 1-15*)
let rec minmax (l : 'a list): 'a * 'a =
  match l with
    |x :: [] -> x, x
    |x :: tl -> let mi, ma = minmax tl in min x mi, max x ma
    |_ -> failwith("liste vide")
    
(*R. 1-16*)
let rec minmax2 (l : 'a list): 'a * 'a =
  let rec minmaux (l1 : 'a list) (mi : 'a) (ma :'a): 'a * 'a =
    match l1 with
      |x :: tl -> if x < mi then minmaux tl x ma
      else if x > ma then minmaux tl mi x
      else minmaux tl mi ma
      |_ -> mi, ma
  in
  match l with
    |x :: tl -> minmaux l x x
    |_ -> failwith("liste vide")

(*R. 1-21*)
let rec tri_fusion (l : 'a list): 'a list =
  (*fonction de fusion*)
  let rec fusion (l1 : 'a list) (l2 : 'a list) =
    match l1, l2 with
      |[], [] -> []
      |_, [] -> l1
      |[], _ -> l2
      |x :: tl1, y :: tl2 ->
        if x < y then x :: fusion tl1 l2
        else y :: fusion l1 tl2
  in
  (*fonction de devision*)
  let rec diviser (l : 'a list): 'a list * 'a list =
    match l with
      |[] -> [], []
      |[x] -> l, []
      |x :: y :: tl -> let lg, ld = diviser tl in x :: lg, y:: ld
  in
  (*fonction principale*)
  match l with
    |[] | _ :: [] -> l
    |_ ->
      let l1, l2 = diviser l in
      let l1s, l2s = tri_fusion l1, tri_fusion l2 in
      fusion l1s l2s
(*R. 1-22*)
let test0 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: []
let test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: []
let tasty = test0 :: test2 :: []
let () = assert(der_el test0 = 42)
let () = assert(der_el2 test0 = Some 42)
let () = assert(miroir test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])
let () = assert(miroir2 test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])
let () = assert(conc test0 test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(conc2 test0 test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(thanos_snap test0 42 = 3 :: 4 :: 67 :: 32 :: 90 :: [])
let () = assert(applat tasty = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(tri_fusion test0 = 3 :: 4 :: 32 :: 42 :: 67 :: 90 :: [])