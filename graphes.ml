(*prints*)
let print_list(l: int list): unit =
  Printf.printf "[";
  let rec aux(l: int list): unit =
    match l with
    |x :: tl -> Printf.printf "%d, " x; aux tl
    |[] -> Printf.printf "]\n"
  in aux l

(*types*)
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
let parcours_prof_rec (g: graphe_adj): int list =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let sl = ref [] in
    let rec parcaux (i: int): unit=
        if atraiter.(i) then begin
            atraiter.(i) <- false;
            sl := i :: !sl;
            List.iter (fun x -> parcaux x) g.(i)
        end in
    for i = 0 to n-1 do 
        parcaux i 
    done;
    List.rev !sl

(*R.6-12*)
let parcours_prof (g: graphe_adj): int list =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let sl = ref [] in
    let p = Stack.create () in
    for i = 0 to n-1 do 
        Stack.push i p;
        while not (Stack.is_empty p) do 
            let s = Stack.pop p in
            if atraiter.(s) then begin
                atraiter.(s) <- false;
                sl := i :: !sl;
                List.iter (fun x -> Stack.push x p) g.(s)
            end
        done
    done;
    List.rev !sl

(*R.6-13*)
let parcours_larg (g: graphe_adj): int list =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let sl = ref [] in
    let p = Queue.create () in
    for i = 0 to n-1 do 
        Queue.push i p;
        while not (Queue.is_empty p) do 
            let s = Queue.pop p in
            if atraiter.(s) then begin
                atraiter.(s) <- false;
                sl := i :: !sl;
                List.iter (fun x -> Queue.push x p) g.(s)
            end
        done
    done;
    List.rev !sl

(*R.6-14*)
let est_acces (g: graphe_adj)(i: int)(j: int): bool =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let rec parcaux (s: int): bool=
        if atraiter.(s) then begin
            atraiter.(s) <- false;
            if s = j then true
            else
            List.fold_left (fun acc x -> acc || parcaux x) false g.(s)
        end
        else false in
    parcaux i

(*R.6-15*)
let est_connexe_no (g: graphe_adj): bool =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let rec parcaux (i: int): unit=
        if atraiter.(i) then begin
            atraiter.(i) <- false;
            List.iter (fun x -> parcaux x) g.(i)
        end in
    for i = 0 to n-1 do 
        parcaux i 
    done;
    (n = 0) || not (Array.mem true atraiter)

(*R.6-17*)
let decomp_connexe_no (g: graphe_adj): int array =
    let n = Array.length g in
    let connexe = Array.make n (-1) in
    let rec parcaux (i: int)(s: int): unit=
        if connexe.(s) = -1 then begin
            connexe.(s) <- i;
            List.iter (fun x -> parcaux i x) g.(s)
        end in
    for i = 0 to n-1 do 
        parcaux i i
    done;
    connexe

(*R.6-19*)
exception Trouve of int
let dist (g: graphe_adj)(i: int)(j: int): int =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let p = Queue.create () in
    try
    Queue.push (i, 0) p;
    while not (Queue.is_empty p) do 
        let s, d = Queue.pop p in
        if atraiter.(s) then begin
            atraiter.(s) <- false;
            if s = j then raise (Trouve d) else
            List.iter (fun x -> Queue.push (x, d + 1) p) g.(s)
        end
    done;
    failwith "sommets inaccessibles"
    with
    |Trouve x -> x

(*R.6-20*)
let tri_topo (g: graphe_adj): int list =
    let n = Array.length g in
    let atraiter = Array.make n true in
    let sl = ref [] in
    let rec parcaux (i: int): unit=
        if atraiter.(i) then begin
            atraiter.(i) <- false;
            List.iter (fun x -> parcaux x) g.(i);
            sl := i :: !sl
        end in
    for i = 0 to n-1 do 
        parcaux i 
    done;
    !sl

(*R.6-21*)
exception ACircuit
let acircuit (g: graphe_adj): bool =
    let n = Array.length g in
    let atraiter = Array.make n None in
    let rec parcaux (i: int): bool=
        match atraiter.(i) with
        |None -> atraiter.(i) <- Some true; 
            let bole = List.fold_left (fun acc x -> acc || parcaux x) false g.(i) in
            atraiter.(i) <- Some false; bole 
        |Some b -> b
        in
    try
    for i = 0 to n-1 do 
        if parcaux i then raise ACircuit
    done;
    false
    with
    |ACircuit -> true

(*R.6-22*)
exception Circuit of int list
let circuiteur (g: graphe_adj): int list option =
    let n = Array.length g in
    let atraiter = Array.make n None in
    let sl = ref [] in
    let rec parcaux (i: int): bool=
        match atraiter.(i) with
        |None -> atraiter.(i) <- Some true; 
            let bole = List.fold_left (fun acc x -> acc || parcaux x) false g.(i) in
            if bole then begin 
                match atraiter.(i) with
                |None -> raise (Circuit !sl)
                |_ -> sl := i :: !sl
            end;
            atraiter.(i) <- Some false; bole 
        |Some b -> if b then (atraiter.(i) <- None; sl := i :: !sl);
            b
        in
    try
    for i = 0 to n-1 do 
        sl := [];
        ignore (parcaux i)
    done;
    None
    with
    |Circuit tl -> Some tl

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
