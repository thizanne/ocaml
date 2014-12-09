let f x y = match x,y with
| 0 .. 1,true -> 1
| 1 .. 2,false -> 2
| _ -> 3

let () =
  assert (f 1 false = 2) ;
  assert (f 1 true = 1) ;
  assert (f 2 false = 2) ;
  assert (f 2 true = 3) ;
  assert (f 0 true = 1) ;
  assert (f 0 false = 3) ;
  ()

let f x = match x with
| 0 -> 0
| _ .. 0 -> -1
| 0 .. _ -> 1


let () =
  assert (f 0 = 0) ;
  assert (f max_int = 1) ;
  assert (f min_int = -1) ;
  ()

let f c = match c with
| 0 .. 1 -> 1
| 1 .. 2 -> 2
| 0 .. 2 -> 3 (* UNUSED !! *)
| _ -> -1

let () =
  assert (f 0 = 1) ;
  assert (f 1 = 1) ;
  assert (f 2 = 2) ;
  assert (f 3 = -1) ;
  ()

(* Disjoint *)
let f c = match c with
| 0 .. 99 -> 1
| 200 .. 299 -> 2
| _ -> 3

let () =
  assert (f 0 = 1) ;
  assert (f 50 = 1) ;
  assert (f 99 = 1) ;
  assert (f 100 = 3) ;
  assert (f 150 = 3) ;
  assert (f 199 = 3) ;
  assert (f 200 = 2) ;
  assert (f 250 = 2) ;
  assert (f 299 = 2) ;
  assert (f 300 = 3) ;
  assert (f (-1) = 3) ;
  ()


let f x = match x with
| 20 .. 30 -> 7
| 1 -> 1
| 2 -> 2
| 4 .. 5 -> 3
| -1 .. 9 -> 4
| 0 .. 100 -> 5
| _ -> 6

let () =
  assert ( f 1 = 1) ;
  assert ( f 2 = 2) ;
  assert ( f 3 = 4) ;
  assert ( f 4 = 3) ;
  assert ( f 5 = 3) ;
  assert ( f 6 = 4) ;
  assert ( f (-1) = 4) ;
  assert ( f 9 = 4) ;
  assert ( f 10 = 5) ;
  assert ( f 19 = 5) ;
  assert ( f 20 = 7) ;
  assert ( f 30 = 7) ;
  assert ( f 31 = 5) ;
  assert ( f 99 = 5) ;
  assert ( f (-2) = 6) ;
  assert ( f 101 = 6) ;
  ()

let f x = match x with
| 0 .. 10 -> 1
| 10 .. 20 -> 2
| 20 .. 30 -> 3
| 30 .. 40 -> 4
| 40 .. 50 -> 5
| 0 .. _   -> 6
| _ .. -1  -> 7

 
let f x = match x with
| 0 .. 1 -> 1
| 2 .. 3 -> 2
| 4 .. 5 -> 3
| 6 .. 7 -> 4
| 8 .. 9 -> 5
| _ -> 6

 
let f x = match x with
| _ .. 'A' -> 1
| 'A' .. 'Z' -> 2
| 'a' .. 'z' -> 3

let () =
  assert ( f '@' = 1) ;
  assert ( f 'A' = 1) ;
  assert ( f 'B' = 2) ;
  assert ( f 'a' = 3) ;
  assert ( f 'Z' = 2) ;
  assert ( try ignore (f '[') ; false with _ -> true) ;
  ()

(* was crashing compiler *)
let f name arg =
  let desc = arg in
  match name, desc with
  | ("+", 1)
  | (("+"|"+."),2) -> 0
  | _ -> 1

let () = 
  assert (f "+" 1 = 0) ;
  assert (f "+." 2 = 0) ;
  assert (f "+" 2 = 0) ;
  assert (f "+" 0 = 1) ;
  ()

let f x = match x with
| _ .. -1,true -> 1
| 0,(true|false) -> 2
| 1 .. _,(true|false) -> 3
| _ .. -2,false -> 4


let () = 
  assert (f (-1,true) = 1) ;
  assert (f (0,true) = 2) ;
  assert (f (1,true) = 3) ;
  assert (f (-10,true) = 1) ;
  assert (f (10,true) = 3) ;
  ()

(* Unsufficient:
File "w.ml", line 1, characters 10-113:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
(-1, _)
*)


let f x y = match x,y with
| ('0', 'x') | ('0', 'X') -> 1
| ('0', 'o') | ('0', 'O') -> 2
| ('0', 'b') | ('0', 'B') -> 3
| _ -> 4
;;

let () =
  assert (f '0' 'x' = 1) ;
  assert (f '0' 'O' = 2) ;
  assert (f '0' 'b' = 3) ;
  assert (f '0' 'z' = 4) ;
  ()

let f x y =  match x,y with
| 0 .. 1,true -> 1
| _     ,true -> 2
| 0 .. 1,false -> 3
| _,false -> 4

let g x y =  match x,y with
| 0,true -> 1
| _     ,true -> 2
| 0,false -> 3
| _,false -> 4

let h x y =  match x,y with
| (0|1),true -> 1
| _     ,true -> 2
| (0|1),false -> 3
| _,false -> 4

let () = Printf.printf "All tests passed\n"
