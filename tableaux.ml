(*R.2-1*)
let prem_zero (tab : int array): int =
    let n = Array.length tab in
    let i = ref 0 in
    while(!i < n && tab.(!i) <> 0) do
        incr i
    done;
    if (!i >= n) then -1 else !i
(*R.2-2*)
exception Found of int
let prem_zero2 (tab : int array) : int =
    let n = Array.length tab in
    try
        for i = 0 to (n-1) do
            if tab.(i) = 0 then raise (Found(i))
        done; -1
    with
        |Found(i)-> i

(*R.2-3*)
let der_zero (tab : int array) : int =
    let n = ref ((Array.length tab) - 1) in
    while (!n >= 0 && tab.(!n) <> 0) do
        n := !n - 1;
    done;
    !n
(*R.2-4*)
(*R.2-5*)
(*R.2-8*)
(*R.2-10*)
(*R.2-15*)
(*R.2-16*)
(*R.2-21*)
(*R.2-22*)
(*R.2-23*)
(*R.2-24*)
(*R.2-25*)

let tab = [|3; 5; 4; 0; 1; 2; 0; 9; 8|]
let () = assert(der_zero tab = 6)