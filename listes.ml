

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
  

let test0 = 3 :: 4 :: 67 :: 32 :: 90 :: 42 :: []
let () = assert(der_el test0 = 42)
let () = assert(der_el2 test0 = Some 42)
let () = assert(miroir test0 = 42 :: 90 :: 32 :: 67 :: 4 :: 3 :: [])