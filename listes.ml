

let rec der_el (l: 'a list) : 'a = 
  match l with 
  |[x] -> x
  |x :: t -> der_el t
  |[] -> raise(Invalid_argument("ta liste est vide"))

let rec der_el2 (l: 'a list) : 'a option =
  match l with
  |[x] -> Some(x)
  |_ :: l2 -> der_el2 l2
  |_ -> None

let miroir (l : 'a list): 'a list =
  let rec miraux (l: 'a list) (lf: 'a list): 'a list =
    match l with
    |x :: l2 -> miraux l2 (x :: lf)
    |[] -> lf
  in miraux l []

let miroir2 (l : 'a list): 'a list = 
  let addup (l2 : 'a list) (x : 'a) : 'a list = x :: l2 
  in List.fold_left (addup) [] l

let rec conc (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
  |x :: tl -> x :: (conc tl l2)
  |[] -> l2 
  
let rec conc2 (l1 : 'a list) (l2 : 'a list) : 'a list =
  let addup (l3 : 'a list) (x : 'a) : 'a list = x :: l3
  in List.fold_left (addup) l1 (miroir l2)
  
let rec thanos_snap (l : 'a list) (x : 'a) : 'a list =
  match l with
  |a :: tl -> if a = x then thanos_snap tl x else a :: thanos_snap tl x
  |[] -> []

let test0 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: []
let test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: []
let () = assert(der_el test0 = 42)
let () = assert(der_el2 test0 = Some 42)
let () = assert(miroir test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])
let () = assert(miroir2 test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])
let () = assert(conc test0 test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: [])
let () = assert(conc2 test0 test2 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: [])
let () = assert(thanos_snap test0 42 = 3 :: 4 :: 67 :: 32 :: 90 :: [])