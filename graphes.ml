type graphe_adj = (* graphes par table de listes d'adjacence *)
int list array

type graphe_mat = (* graphes par matrice d'adjacence *)
bool array array

type graphe_mat_pond = (* graphes par matrice d'adjacence *)
int array array

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