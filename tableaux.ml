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
let is_there_zero (tab : int array) : bool =
    let n = Array.length tab in
    let i = ref 0 in
    while (!i < (n-1) && tab.(!i) <> 0) do 
        i := !i + 1
    done;
    tab.(!i) = 0
(*R.2-5*)
exception Foundi
let is_there_zero2 (tab : int array) : bool =
    let test (x : int) : unit = if x = 0 then raise Foundi in
    try 
        Array.iter (test) tab;
        false
    with
        |Foundi -> true

(*R.2-8*)
let min (tab : int  array) : int =
    let n = Array.length tab in
    if n = 0 then raise (Invalid_argument "tableau de taille 0") else
    let cache = ref tab.(0) in
    let icache = ref 0 in
    for i = 1 to (n-1) do
        if tab.(i) < !cache then 
            begin cache := tab.(i); icache := i end
    done;
    !icache
(*R.2-10*)
let accu (tab : int array) : int array =
    let res = Array.make (Array.length tab) tab.(0) in
    for i = 1 to ((Array.length tab) -1) do
        begin
            res.(i) <- tab.(i)  + res.(i-1)
        end
    done;
    res
(*R.2-15*)
let decale (tab : int array) (g : int) (d : int) : unit =
    let i = ref d in
    while !i > g do
        tab.(!i) <- tab.(!i-1);
        i := !i - 1
    done
(*R.2-16*)
let tri_inser (tab : int array) : unit =
    for i = 1 to ((Array.length tab) - 1) do
        let pos = ref i in
        let cache = tab.(i) in
        while (!pos <> 0 && tab.(!pos - 1) > cache) do
            tab.(!pos) <- tab.(!pos-1);
            pos := !pos - 1
        done;
        tab.(!pos) <- cache
    done
(*R.2-21*)
let list_to_string (l : char list): string =
    let rec straux (l : char list) (s : string): string =
        match l with
            |x :: tl -> straux tl (s ^ String.make 1 x)
            |_ -> s
    in straux l ""
(*R.2-22*)
let string_to_list (s : string): char list =
    List.rev (String.fold_left (fun acc el -> el :: acc) [] s)
(*R.2-23*)
exception Lettre 
let string_to_int (s : string): int option=
    let acc = ref ((int_of_char s.[0])  - (int_of_char '0')) in
    let n = String.length s in
    try
        for i = 1 to (n-1) do
            if s.[i] >= '0'&& s.[i] <='9'then
                acc := !acc * 10 + (int_of_char s.[i]) - (int_of_char '0')
            else
                raise(Lettre)
        done;
        Some !acc
    with
        |Lettre -> None
(*R.2-24*)
let dichot (t : int array) (p : int): bool =
    let rec dicaux (t : int array) (p : int) (mini : int) (maxi :int): bool =
        if mini >= maxi && not (p = t.(mini)) then false else
        let mid = (mini + maxi) / 2 in
        if p = t.(mid) then
            true
        else if p < t.(mid) then
            dicaux t p mini mid
        else 
            dicaux t p mid maxi
    in dicaux t p 0 ((Array.length t) - 1)
(*R.2-25*)
let dichot2 (t : int array) (p : int): bool =
    let mini, maxi = ref 0, ref ((Array.length t) - 1)in
    let mid = ref ((!mini + !maxi) / 2) in
    while !mini < !maxi && not (p = t.(!mid)) do
        mid := (!mini + !maxi) / 2;
        if p < t.(!mid) then
            maxi := !mid -1 
        else 
            mini := !mid + 1
    done;
    p = t.(!mid)

let rec expo (n : int) (x : int) : int =
    if n = 0 then
        1
    else
    let oui = (expo (n/2) x) in
    if n mod 2 = 0 then
        oui * oui
    else
        oui * oui * x





let tab = [|3; 5; 4; 0; 1; 2; 0; 9; 8|]
let tab1 = [|3; 5; 4; 4; 1; 2; 6; 9; 8|]
let mot = ['o'; 'u'; 'i']
let mots = "oui"
let nombre = "123"
let () = assert(der_zero tab = 6)
let () = assert(is_there_zero tab)
let () = assert(not (is_there_zero tab1))
let () = assert(is_there_zero2 tab)
let () = assert(not (is_there_zero2 tab1))
let () = assert(min tab1 = 4)
let () = assert(accu tab = [|3; 8; 12; 12; 13; 15; 15; 24; 32|])
let () = decale tab1 1 6
let () = assert(tab1 = [|3; 5; 5; 4; 4; 1; 2; 9; 8|])
let () = tri_inser tab
let () = assert(tab = [|0; 0; 1; 2; 3; 4; 5; 8; 9|])
let () = assert(string_to_list mots = mot)
let () = assert(list_to_string mot = mots)
let () = let f x = match x with |Some a -> a |_ -> -1 in print_int(f (string_to_int nombre))
let () = assert(string_to_int nombre = Some 123)
let () = assert(dichot tab 1)
let () = assert(dichot2 tab 1)