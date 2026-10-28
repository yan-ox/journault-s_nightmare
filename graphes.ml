type graphe_adj = (* graphes par table de listes d'adjacence *)
int list array

type graphe_mat = (* graphes par matrice d'adjacence *)
bool array array

type graphe_mat_pond = (* graphes par matrice d'adjacence *)
int array array

(*R.6-1*)
let est_arrete_mat (g: graphe_mat)(i: int)(j: int): bool = 
    g.(i).(j)

(*R.6-2*)
let est_arrete_adj (g: graphe_adj)(i: int)(j: int): bool =
    List.mem j g.(i)

(*R.6-3*)
let creer_adj_o (n: int)(l: (int * int) list): graphe_adj =
    let g = Array.make n [] in
    let rec creaux (l: (int * int) list): unit =
        match l with
        |[] -> ()
        |(x, y) :: tl when x < n && y < n -> g.(x) <- y :: g.(x); creaux tl
        |_ -> raise(Invalid_argument "arrête impossible") in
    creaux l;
    g

(*R.6-4*)
let creer_adj_no (n: int)(l: (int * int) list): graphe_adj =
    let g = Array.make n [] in
    let rec creaux (l: (int * int) list): unit =
        match l with
        |[] -> ()
        |(x, y) :: tl when x < n && y < n -> g.(x) <- y :: g.(x); g.(y) <- x :: g.(y); creaux tl
        |_ -> raise(Invalid_argument "arrête impossible") in
    creaux l;
    g

(*R.6-5*)
let creer_mat_pond (n: int)(l: (int * int) list): graphe_mat =
    let g = Array.make_matrix n n false in
    let rec creaux (l: (int * int) list): unit =
        match l with
        |[] -> ()
        |(x, y) :: tl when x < n && y < n -> g.(x).(y) <- true; creaux tl
        |_ -> raise(Invalid_argument "arrête impossible") in
    creaux l;
    g

(*R.6-6*)
let list_arc_adj (g: graphe_adj): (int * int) list =
    let l = ref [] in
    for i = 0 to (Array.length g - 1) do 
        List.iter (fun x -> l := (i, x) :: !l) g.(i)
    done;
    !l

(*R.6-7*)
let list_arc_mat (g: graphe_mat): (int * int) list =
    let l = ref [] in
    for i = 0 to (Array.length g - 1) do 
        for j = 0 to (Array.length g - 1) do 
            if g.(i).(j) then l := (i, j) :: !l
        done
    done;
    !l

(*R.6-10*)
let addf (a : int) (b : int): int=
    if a = -1 || b = -1 then
        -1
    else a + b
let minf (a : int) (b : int): int=
    if a = -1 then
        b
    else if b = -1 then
        a
    else
        min a b

let floyd_marshmall (g : graphe_mat_pond): graphe_mat_pond =
    let n = Array.length g in
    let old = ref (Array.map Array.copy g) in
    for i = 0 to (n-1) do !old.(i).(i) <- 0 done;
    for k = 0 to (n-1) do
        for i = 0 to (n-1) do
            for j = 0 to (n-1) do
                !old.(i).(j) <- minf (addf !old.(i).(k) !old.(k).(j)) !old.(i).(j);
            done;
        done;
    done;
    !old

(*R.6-11*)
(*R.6-12*)
(*R.6-13*)
(*R.6-14*)
(*R.6-15*)
(*R.6-17*)
(*R.6-19*)
(*R.6-20*)
(*R.6-21*)
(*R.6-22*)

(*test*)
let g1 : graphe_mat_pond =
  [|
    [|-1; 5; 2|];  (* arcs depuis 0 vers 0,1,2 *)
    [|-1; -1; 3|];  (* arcs depuis 1 *)
    [|-1; -1; -1|]   (* arcs depuis 2 *)
  |]
let g2 : graphe_mat_pond =
  [|
    [|-1; 4; -1; -1|];  (* depuis 0 *)
    [|4; -1; 1; -1|];  (* depuis 1 *)
    [|-1; 1; -1; 7|];  (* depuis 2 *)
    [|-1; -1; 7; -1|]   (* depuis 3 *)
  |]
let g3 : graphe_mat_pond =
  [|
    [|-1; 2; 9|];
    [|3; -1; 4|];
    [|1; 6; -1|]
  |]