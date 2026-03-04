(* TEST
 flags = "-dlambda -dcanonical-ids";
 expect;
*)

(* Small int intervals: compiled as range checks via Switcher *)
let f x = match x with
  | 0 .. 5 -> 1
  | 6 .. 10 -> 2
  | _ -> 0
;;
[%%expect {|
(let
  (f/0 =
     (function x/0[int] : int
       (catch
         (if (>= x/0 6) (if (>= x/0 11) (exit 2) 2)
           (if (>= x/0 0) 1 (exit 2)))
        with (2) 0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))
val f : int -> int = <fun>
|}]

(* Large int intervals: bound-check chains *)
let g x = match x with
  | 0 .. 1000 -> 1
  | 1001 .. 2000 -> 2
  | _ -> 0
;;
[%%expect {|
(let
  (g/0 =
     (function x/1[int] : int
       (if (&& (<= 0 x/1) (<= x/1 1000)) 1
         (if (&& (<= 1001 x/1) (<= x/1 2000)) 2 0))))
  (apply (field_mut 1 (global Toploop!)) "g" g/0))
val g : int -> int = <fun>
|}]

(* Int32 intervals: Pbintcomp bound checks *)
let h (x : int32) = match x with
  | 0l .. 10l -> 1
  | 11l .. 20l -> 2
  | _ -> 0
;;
[%%expect {|
(let
  (h/0 =
     (function x/2[int32] : int
       (if (&& (Int32.<= 0l x/2) (Int32.<= x/2 10l)) 1
         (if (&& (Int32.<= 11l x/2) (Int32.<= x/2 20l)) 2 0))))
  (apply (field_mut 1 (global Toploop!)) "h" h/0))
val h : int32 -> int = <fun>
|}]

(* 4+ boxed int intervals: binary search tree *)
let k (x : int32) = match x with
  | 0l .. 100l -> 1
  | 101l .. 200l -> 2
  | 201l .. 300l -> 3
  | 301l .. 400l -> 4
  | _ -> 0
;;
[%%expect {|
(let
  (k/0 =
     (function x/3[int32] : int
       (catch
         (if (Int32.< x/3 201l)
           (if (&& (Int32.<= 0l x/3) (Int32.<= x/3 100l)) 1
             (if (&& (Int32.<= 101l x/3) (Int32.<= x/3 200l)) 2 (exit 13)))
           (if (&& (Int32.<= 201l x/3) (Int32.<= x/3 300l)) 3
             (if (&& (Int32.<= 301l x/3) (Int32.<= x/3 400l)) 4 (exit 13))))
        with (13) 0)))
  (apply (field_mut 1 (global Toploop!)) "k" k/0))
val k : int32 -> int = <fun>
|}]

(* Overlapping intervals: arms whose ranges intersect must not share a
   dispatch. They are compiled as separate groups chained by exits, in
   source order, so that a value in the overlap that fails the rest of
   an earlier row falls through to the later row (first-match-wins). *)
let m x y = match x, y with
  | 0 .. 10, true -> 1
  | 5 .. 15, _ -> 2
  | _ -> 0
;;
[%%expect {|
(let
  (m/0 =
     (function x/4[int] y/0[int] : int
       (catch (if (isout 10 x/4) (exit 17) (if y/0 1 (exit 17))) with (17)
         (if (isout 10 (-5+ x/4)) 0 2))))
  (apply (field_mut 1 (global Toploop!)) "m" m/0))
val m : int -> bool -> int = <fun>
|}]
