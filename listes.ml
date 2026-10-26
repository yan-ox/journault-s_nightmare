(*prints*)
let print_list(l: int list): unit =
  Printf.printf "[";
  let rec aux(l: int list): unit =
    match l with
    |x :: tl -> Printf.printf "%d, " x; aux tl
    |[] -> Printf.printf "]\n"
  in aux l

let print_listlist(l: int list list): unit =
  Printf.printf "[";
  let rec petitaux(l: int list): unit =
    match l with
    |x :: tl -> Printf.printf "%d, " x; petitaux tl
    |[] -> Printf.printf "]"
  in
  let rec grosaux (l: int list list): unit =
    match l with
    |x :: tl -> Printf.printf "["; petitaux x; grosaux tl
    |[] -> Printf.printf "]\n"
  in grosaux l

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

(*R. 1-10*)
let segregation (l: 'a list): 'a list * 'a list =
  let rec segraux (l: 'a list)(l0: 'a list)(l1: 'a list)(cpt: int): 'a list * 'a list=
    match l with
    |x :: tl -> if cpt mod 2 = 0 then segraux tl (x :: l0) l1 (cpt + 1)
      else segraux tl l0 (x :: l1) (cpt + 1)
    |_ -> List.rev l0, List.rev l1
  in segraux l [] [] 0

(*R. 1-11*)
let applat (l: 'a list list): 'a list =
  let rec applaux (l1 : 'a list list) (lp : 'a list): 'a list =
    match l1 with
      |x :: tl -> applaux tl ((List.rev x) @ lp)
      |[] -> lp
  in List.rev (applaux l [])

let rec apaugustin (l: 'a list list): 'a list =
  match l with
    |[] -> []
    |x :: tl -> x @ apaugustin tl

(*R. 1-12*)
let applat2 (l: 'a list list): 'a list =
  let rec aux1 (ll: 'a list list)(sl: 'a list): 'a list =
    match ll with
    |[] -> List.rev sl
    |x :: tl -> aux2 tl x sl

  and aux2 (ll: 'a list list)(l: 'a list)(sl: 'a list): 'a list =
    match l with
    |[] -> aux1 ll sl
    |x :: tl -> aux2 ll tl (x :: sl)
  in aux1 l []

(*R. 1-13*)
let applat3 (l : 'a list list): 'a list =
  let rec applaux (l : 'a list list)(sl : 'a list): 'a list =
    match l with
    |[] -> List.rev sl
    |pl :: tgl -> begin
      match pl with
      |[] -> applaux tgl sl
      |x :: tpl -> applaux (tpl :: tgl) (x :: sl)
    end
  in applaux l []

(*R. 1-14*)
let applat4 (l : 'a list list): 'a list =
  List.rev (List.fold_left (fun acc1 pl -> List.fold_left (fun acc2 x -> x :: acc2) acc1 pl) [] l)

(*R. 1-15*)
let rec minmax (l : 'a list): 'a * 'a =
  match l with
    |x :: [] -> x, x
    |x :: tl -> let mi, ma = minmax tl in min x mi, max x ma
    |_ -> raise(Invalid_argument "liste vide")
    
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
    |_ -> raise(Invalid_argument "liste vide")

(*R. 1-17*)
let minmax3 (l : 'a list): 'a * 'a =
    match l with
    |[] -> raise(Invalid_argument "liste vide")
    |p :: tl -> List.fold_left (fun acc x -> 
      let mi, ma = acc in (min x mi, max x ma)) (p, p) tl

(*R. 1-18*)
let souseq_crois (l : int list): int list list =
  let rec sousaux (l : int list)(sl : int list): int list * int list =
    match l, sl with
    |x :: tl, y :: _  when x >= y -> sousaux tl (x :: sl)
    |x :: tl, [] -> sousaux tl (x :: sl)
    |_ -> l , List.rev sl
  in let rec grosaux (l : int list)(sll : int list list):int list list =
    match l with
    |[] -> List.rev sll
    |_ -> let l1, l2 = sousaux l [] in grosaux l1 (l2 :: sll)
  in grosaux l []

(*R. 1-19*)
let souseq_mono (l : int list): int list list =
  let rec sousaux (l : int list)(sl : int list)(b: bool option): int list * int list =
    match b with
    |None -> begin
      match l, sl with
      |x :: tl, y :: _ when x = y -> sousaux tl (x :: sl) b
      |x :: tl, y :: _ -> sousaux tl (x :: sl) (Some (x < y))
      |x :: tl, [] -> sousaux tl (x :: sl) b
      |_-> l, List.rev sl
    end
    |Some true -> begin
      match l, sl with
      |x :: tl, y :: _  when x <= y -> sousaux tl (x :: sl) b
      |x :: tl, [] -> sousaux tl (x :: sl) b (*useless*)
      |_ -> l , List.rev sl
    end
    |Some false -> begin
      match l, sl with
      |x :: tl, y :: _  when x >= y -> sousaux tl (x :: sl) b
      |x :: tl, [] -> sousaux tl (x :: sl) b (*useless*)
      |_ -> l , List.rev sl
    end
  in let rec grosaux (l : int list)(sll : int list list):int list list =
    match l with
    |[] -> List.rev sll
    |_ -> let l1, l2 = sousaux l [] None in grosaux l1 (l2 :: sll)
  in grosaux l []

(*prof*)
type signe = bool option
(* Some(true) indique une séquence croissante, Some(false) une décroiss. et
None une suite ni strictement croiss, ni strict décroiss, ie cst *)

let sous_sequences_monotones (l: 'a list): ('a list list) =
(* Décompose la liste l est une liste de sous-séquences croissantes. *)
  let diff_compatible_avec_signe (s: signe) (a:'a) (b:'a) : bool =
   (* Teste si [a; b] est compatible avec la monotonie indiquée par s *)
   s = None || (a <= b && s = Some true) || (a >= b && s = Some false) in
 let raffine_avec_diff (s: signe) (a: 'a) (b: 'a) : signe =
   (* Raffine la monotonie s afin qu'elle tienne compte de [a; b] *)
   if a = b then s else if a < b then Some true else Some false in
 let rec aux
   (todo: 'a list) (lres: 'a list list)
   (accu: 'a list) (csigne: signe) : 'a list list =
   (* todo est la liste des éléments encore à traiter. lres est la liste
   des sous séquences monotones déjà construites avec le début de l.
   accu est la sous-séquence monotone en cours de construction, elle
   est de monotonie indiquée par csigne *)
   match todo, accu with
   | [], [] -> []
   | [], _ -> List.rev ((List.rev accu) :: lres)
   | next::todo', [] -> aux todo' lres (next::accu) None
   | next::todo', last::accu' ->
   if diff_compatible_avec_signe csigne last next
   then aux todo' lres (next::accu) (raffine_avec_diff csigne last next)
   else aux todo' ((List.rev accu) :: lres) [next] None
 in aux l [] [] None
 
(*R. 1-20*)
let sort_map(l: 'a list)(k : int)(f: 'a->int): 'a list=
  let arr = Array.make (k + 1) [] in
  List.iter (fun x -> let i = f x in arr.(i) <- x :: arr.(i)) l;
  let sl = Array.to_list arr in List.flatten sl

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
type 'a file = 'a list * 'a list

let file_vide: 'a file = ([], [])

let est_vide(f: 'a file):bool = f = ([], [])

let enfile (e: 'a) (f: 'a file): 'a file=
  let (g, d) = f in (e :: g, d)

let rec defile (f: 'a file): 'a *'a file =
  match f with
    |[], [] -> failwith "pile"
    |g, x :: d' -> x, (g, d')
    |g, [] -> defile([], List.rev g)




(*tests*)
let test0 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: []
let test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: []
let tasty = test0 :: test2 :: []
let raaaah = [1; 4; 5; 8; 7; 5; 2; 3; 4; 9; 3; 3; 3; 5; 5; 7; 2]
let () = assert(der_el test0 = 42)
let () = assert(der_el2 test0 = Some 42)
let () = assert(miroir test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])
let () = assert(miroir2 test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])
let () = assert(conc test0 test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(conc2 test0 test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(thanos_snap test0 42 = 3 :: 4 :: 67 :: 32 :: 90 :: [])
let () = assert(applat tasty = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(tri_fusion test0 = 3 :: 4 :: 32 :: 42 :: 67 :: 90 :: [])
let () = assert(segregation test0 = (3 :: 67 :: 90 :: [], 4 :: 32 :: 42 :: []))
let () = assert(souseq_crois test0 = [[3; 4; 67]; [32; 90]; [42]])
let () = assert(applat2 tasty = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(applat3 tasty = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(applat4 tasty = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 43 :: [])
let () = assert(souseq_crois raaaah = [[1; 4; 5; 8]; [7]; [5]; [2; 3; 4; 9]; [3; 3; 3; 5; 5; 7]; [2]])
let () = assert(souseq_mono raaaah = [[1; 4; 5; 8]; [7; 5; 2]; [3; 4; 9]; [3; 3; 3; 5; 5; 7]; [2]])
