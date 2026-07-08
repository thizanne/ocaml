(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

(****************************************************************)
(*  Tests for interval/range patterns in pattern matching       *)
(****************************************************************)

let test msg f arg r =
  if f arg <> r then begin
    prerr_endline msg ;
    failwith "Malaise"
  end
;;

(* Basic integer intervals *)

let f1 x = match x with
  | 0 .. 10 -> 1
  | 11 .. 100 -> 2
  | _ -> 3
;;

test "int interval 1" f1 0 1;
test "int interval 2" f1 5 1;
test "int interval 3" f1 10 1;
test "int interval 4" f1 11 2;
test "int interval 5" f1 50 2;
test "int interval 6" f1 100 2;
test "int interval 7" f1 101 3;
test "int interval 8" f1 (-1) 3;
()
;;

(* Negative integer intervals *)

let f2 x = match x with
  | -100 .. -1 -> 1
  | 0 -> 2
  | 1 .. 100 -> 3
  | _ -> 4
;;

test "neg interval 1" f2 (-50) 1;
test "neg interval 2" f2 (-1) 1;
test "neg interval 3" f2 0 2;
test "neg interval 4" f2 1 3;
test "neg interval 5" f2 50 3;
test "neg interval 6" f2 200 4;
()
;;

(* Mixed constants and intervals *)

let f3 x = match x with
  | 0 -> 1
  | 1 .. 9 -> 2
  | 10 -> 3
  | _ -> 4
;;

test "mixed 1" f3 0 1;
test "mixed 2" f3 5 2;
test "mixed 3" f3 10 3;
test "mixed 4" f3 20 4;
()
;;

(* Large intervals (> 256, uses bound checks not expansion) *)

let f4 x = match x with
  | 0 .. 1000 -> 1
  | 1001 .. 2000 -> 2
  | _ -> 3
;;

test "large interval 1" f4 500 1;
test "large interval 2" f4 1500 2;
test "large interval 3" f4 2500 3;
()
;;

(* Single-element interval *)

let f5 x = match x with
  | 5 .. 5 -> 1
  | _ -> 2
;;

test "single element 1" f5 5 1;
test "single element 2" f5 4 2;
test "single element 3" f5 6 2;
()
;;

(* Reversed interval (lo > hi, compiler normalizes) *)

let f6 x = match x with
  | 10 .. 0 -> 1
  | _ -> 2
;;

test "reversed 1" f6 5 1;
test "reversed 2" f6 0 1;
test "reversed 3" f6 10 1;
test "reversed 4" f6 11 2;
()
;;

(* Int32 intervals *)

let f7 (x : int32) = match x with
  | 0l .. 100l -> 1
  | 101l .. 200l -> 2
  | _ -> 3
;;

test "int32 1" f7 50l 1;
test "int32 2" f7 150l 2;
test "int32 3" f7 300l 3;
()
;;

(* Int64 intervals *)

let f8 (x : int64) = match x with
  | 0L .. 100L -> 1
  | 101L .. 200L -> 2
  | _ -> 3
;;

test "int64 1" f8 50L 1;
test "int64 2" f8 150L 2;
test "int64 3" f8 300L 3;
()
;;

(* Nativeint intervals *)

let f9 (x : nativeint) = match x with
  | 0n .. 100n -> 1
  | 101n .. 200n -> 2
  | _ -> 3
;;

test "nativeint 1" f9 50n 1;
test "nativeint 2" f9 150n 2;
test "nativeint 3" f9 300n 3;
()
;;

(* Overlapping intervals (should warn about redundancy) *)

let f10 x = match x with
  | 0 .. 10 -> 1
  | 5 .. 15 -> 2
  | _ -> 3
;;

test "overlap 1" f10 3 1;
test "overlap 2" f10 7 1;
test "overlap 3" f10 12 2;
test "overlap 4" f10 20 3;
()
;;

(* Interval with guard *)

let f11 x = match x with
  | 0 .. 10 when x mod 2 = 0 -> 1
  | 0 .. 10 -> 2
  | _ -> 3
;;

test "guard 1" f11 4 1;
test "guard 2" f11 5 2;
test "guard 3" f11 20 3;
()
;;

(* Interval in tuple pattern *)

let f12 (x, y) = match x, y with
  | 0 .. 10, 0 .. 10 -> 1
  | _ -> 2
;;

test "tuple 1" f12 (5, 5) 1;
test "tuple 2" f12 (5, 20) 2;
test "tuple 3" f12 (20, 5) 2;
()
;;

(* Interval in or-pattern *)

let f13 x = match x with
  | (0 .. 10 | 20 .. 30) -> 1
  | _ -> 2
;;

test "or-pat 1" f13 5 1;
test "or-pat 2" f13 15 2;
test "or-pat 3" f13 25 1;
test "or-pat 4" f13 35 2;
()
;;

(* Cross-boundary intervals: lo < 0, hi >= 0 *)

let f14 x = match x with
  | -100 .. 100 -> 1
  | _ -> 2
;;

test "cross-boundary 1" f14 (-100) 1;
test "cross-boundary 2" f14 0 1;
test "cross-boundary 3" f14 100 1;
test "cross-boundary 4" f14 101 2;
()
;;

(* Large intervals spanning sign boundary *)

let f15 x = match x with
  | -1000000 .. 1000000 -> 1
  | _ -> 2
;;

test "large cross 1" f15 0 1;
test "large cross 2" f15 (-1000000) 1;
test "large cross 3" f15 1000000 1;
test "large cross 4" f15 1000001 2;
()
;;

(* Int32 boundary values *)

let f16 (x : int32) = match x with
  | -2147483648l .. 0l -> 1
  | 1l .. 2147483647l -> 2
;;

test "int32 min" f16 (-2147483648l) 1;
test "int32 max" f16 2147483647l 2;
test "int32 zero" f16 0l 1;
()
;;

(* Int64 boundary values *)

let f17 (x : int64) = match x with
  | -9223372036854775808L .. 0L -> 1
  | 1L .. 9223372036854775807L -> 2
;;

test "int64 min" f17 (-9223372036854775808L) 1;
test "int64 max" f17 9223372036854775807L 2;
test "int64 zero" f17 0L 1;
()
;;

(* Many int intervals: triggers binary search (>= 4 entries) *)

let f18 x = match x with
  | 0 .. 100 -> 1
  | 101 .. 200 -> 2
  | 201 .. 300 -> 3
  | 301 .. 400 -> 4
  | 401 .. 500 -> 5
  | _ -> 0
;;

test "bsearch int 1" f18 50 1;
test "bsearch int 2" f18 150 2;
test "bsearch int 3" f18 250 3;
test "bsearch int 4" f18 350 4;
test "bsearch int 5" f18 450 5;
test "bsearch int 6" f18 (-1) 0;
test "bsearch int 7" f18 501 0;
()
;;

(* Many int32 intervals: triggers binary search *)

let f19 (x : int32) = match x with
  | 1l .. 10l -> 1
  | 11l .. 20l -> 2
  | 21l .. 30l -> 3
  | 31l .. 40l -> 4
  | _ -> 0
;;

test "bsearch int32 1" f19 5l 1;
test "bsearch int32 2" f19 15l 2;
test "bsearch int32 3" f19 25l 3;
test "bsearch int32 4" f19 35l 4;
test "bsearch int32 5" f19 0l 0;
test "bsearch int32 6" f19 41l 0;
()
;;

(* Int64 intervals with binary search *)

let f20 (x : int64) = match x with
  | 0L .. 100L -> 1
  | 101L .. 200L -> 2
  | 201L .. 300L -> 3
  | 301L .. 400L -> 4
  | _ -> 0
;;

test "bsearch int64 1" f20 50L 1;
test "bsearch int64 2" f20 150L 2;
test "bsearch int64 3" f20 250L 3;
test "bsearch int64 4" f20 350L 4;
test "bsearch int64 5" f20 (-1L) 0;
()
;;

(* Nativeint intervals with binary search *)

let f21 (x : nativeint) = match x with
  | 0n .. 100n -> 1
  | 101n .. 200n -> 2
  | 201n .. 300n -> 3
  | 301n .. 400n -> 4
  | _ -> 0
;;

test "bsearch nativeint 1" f21 50n 1;
test "bsearch nativeint 2" f21 150n 2;
test "bsearch nativeint 3" f21 250n 3;
test "bsearch nativeint 4" f21 350n 4;
test "bsearch nativeint 5" f21 (-1n) 0;
()
;;

(* Mixed singletons and intervals (int32) *)

let f22 (x : int32) = match x with
  | 0l -> 0
  | 1l .. 10l -> 1
  | 11l -> 2
  | 12l .. 20l -> 3
  | 100l -> 4
  | _ -> 5
;;

test "mixed int32 1" f22 0l 0;
test "mixed int32 2" f22 5l 1;
test "mixed int32 3" f22 11l 2;
test "mixed int32 4" f22 15l 3;
test "mixed int32 5" f22 100l 4;
test "mixed int32 6" f22 50l 5;
()
;;

(* Exactly 3 entries: linear path, no binary split *)

let f23 (x : int32) = match x with
  | 1l .. 10l -> 1
  | 11l .. 20l -> 2
  | 21l .. 30l -> 3
  | _ -> 0
;;

test "linear int32 1" f23 5l 1;
test "linear int32 2" f23 15l 2;
test "linear int32 3" f23 25l 3;
test "linear int32 4" f23 0l 0;
()
;;

(* Exactly 1 entry: singleton path *)

let f24 (x : int64) = match x with
  | 100L .. 200L -> 1
  | _ -> 0
;;

test "single int64 1" f24 150L 1;
test "single int64 2" f24 99L 0;
test "single int64 3" f24 201L 0;
test "single int64 4" f24 100L 1;
test "single int64 5" f24 200L 1;
()
;;

(* Basic char intervals *)

let f25 c = match c with
  | 'a' .. 'z' -> 1
  | '0' .. '9' -> 2
  | 'A' .. 'Z' -> 3
  | _ -> 0
;;

test "char interval 1" f25 'a' 1;
test "char interval 2" f25 'm' 1;
test "char interval 3" f25 'z' 1;
test "char interval 4" f25 '0' 2;
test "char interval 5" f25 '9' 2;
test "char interval 6" f25 'A' 3;
test "char interval 7" f25 'Z' 3;
test "char interval 8" f25 ' ' 0;
()
;;

(* Char boundary values *)

let f26 c = match c with
  | '\000' .. '\031' -> 1   (* control characters *)
  | '\128' .. '\255' -> 2   (* high bytes *)
  | _ -> 0
;;

test "char boundary 1" f26 '\000' 1;
test "char boundary 2" f26 '\031' 1;
test "char boundary 3" f26 '\032' 0;
test "char boundary 4" f26 '\128' 2;
test "char boundary 5" f26 '\255' 2;
test "char boundary 6" f26 '\127' 0;
()
;;

(* Overlapping intervals whose source order disagrees with
   sort-by-lower-bound order.  The bound-check chain in matching.ml
   must preserve first-match-wins semantics, so a value in both
   intervals must select the arm written first. *)

let f27 (x : int32) = match x with
  | 5l .. 15l -> 1
  | 0l .. 10l -> 2
  | _ -> 3
;;

test "int32 overlap 1" f27 (-1l) 3;
test "int32 overlap 2" f27 0l 2;
test "int32 overlap 3" f27 4l 2;
test "int32 overlap 4" f27 5l 1;
test "int32 overlap 5" f27 7l 1;
test "int32 overlap 6" f27 10l 1;
test "int32 overlap 7" f27 15l 1;
test "int32 overlap 8" f27 16l 3;
()
;;

let f28 (x : int64) = match x with
  | 100L .. 300L -> 1
  | 0L .. 200L -> 2
  | _ -> 3
;;

test "int64 overlap 1" f28 50L 2;
test "int64 overlap 2" f28 100L 1;
test "int64 overlap 3" f28 150L 1;
test "int64 overlap 4" f28 250L 1;
test "int64 overlap 5" f28 301L 3;
()
;;

let f29 (x : nativeint) = match x with
  | 5n .. 20n -> 1
  | 0n .. 10n -> 2
  | 15n .. 25n -> 3
  | _ -> 4
;;

test "nativeint overlap 1" f29 0n 2;
test "nativeint overlap 2" f29 4n 2;
test "nativeint overlap 3" f29 5n 1;
test "nativeint overlap 4" f29 12n 1;
test "nativeint overlap 5" f29 20n 1;
test "nativeint overlap 6" f29 21n 3;
test "nativeint overlap 7" f29 25n 3;
test "nativeint overlap 8" f29 26n 4;
()
;;

(* Same scenario for int, but wide enough (span >= 256) to skip
   the small-int switch expansion and hit the bound-check chain. *)

let f30 x = match x with
  | 500 .. 1500 -> 1
  | 0 .. 1000 -> 2
  | _ -> 3
;;

test "int wide overlap 1" f30 (-1) 3;
test "int wide overlap 2" f30 0 2;
test "int wide overlap 3" f30 499 2;
test "int wide overlap 4" f30 500 1;
test "int wide overlap 5" f30 1000 1;
test "int wide overlap 6" f30 1500 1;
test "int wide overlap 7" f30 1501 3;
()
;;

(* More than four mutually overlapping arms. Overlapping arms are
   compiled as separate groups chained by exits, in source order.
   Each test value below would select a different arm if the compiler
   reordered overlapping intervals by lower bound (as an earlier,
   buggy implementation did). *)

let f31 (x : int32) = match x with
  | 50l .. 100l -> 1
  | 30l .. 70l -> 2
  | 10l .. 40l -> 3
  | 0l .. 20l -> 4
  | 90l .. 120l -> 5
  | _ -> 0
;;

test "int32 overlap many 1" f31 (-1l) 0;
test "int32 overlap many 2" f31 5l 4;
test "int32 overlap many 3" f31 15l 3;   (* rows 3 and 4 overlap *)
test "int32 overlap many 4" f31 25l 3;
test "int32 overlap many 5" f31 35l 2;   (* rows 2 and 3 overlap *)
test "int32 overlap many 6" f31 45l 2;
test "int32 overlap many 7" f31 55l 1;   (* rows 1 and 2 overlap *)
test "int32 overlap many 8" f31 65l 1;
test "int32 overlap many 9" f31 95l 1;   (* rows 1 and 5 overlap *)
test "int32 overlap many 10" f31 110l 5;
test "int32 overlap many 11" f31 121l 0;
()
;;

(* Regression tests: overlapping intervals across several columns.
   A buggy implementation grouped overlapping intervals into one
   division keyed by exact bounds; a value entering an earlier
   interval's cell whose remaining columns then failed would skip
   the later overlapping rows and fall to the match default. *)

let f32 x y = match x, y with
  | 0 .. 10, true -> 1
  | 5 .. 15, _ -> 2
  | _ -> 4
;;

test "multi-col overlap 1" (f32 7) true 1;
test "multi-col overlap 2" (f32 7) false 2;
test "multi-col overlap 3" (f32 3) false 4;
test "multi-col overlap 4" (f32 12) false 2;
test "multi-col overlap 5" (f32 12) true 2;
test "multi-col overlap 6" (f32 20) true 4;
()
;;

(* A subset interval first, its superset later *)

let f33 x y = match x, y with
  | 5 .. 10, true -> 1
  | 0 .. 15, _ -> 2
  | _, _ -> 3
;;

test "subset overlap 1" (f33 7) false 2;
test "subset overlap 2" (f33 7) true 1;
test "subset overlap 3" (f33 20) true 3;
()
;;

(* A plain constant overlapping a later interval *)

let f34 x y = match x, y with
  | 7, true -> 1
  | 5 .. 15, _ -> 2
  | _ -> 3
;;

test "const overlap 1" (f34 7) true 1;
test "const overlap 2" (f34 7) false 2;
test "const overlap 3" (f34 9) false 2;
test "const overlap 4" (f34 4) false 3;
()
;;

(* Exhaustive match (no catch-all row) with overlapping intervals *)

let f35 x y = match x, y with
  | 0 .. 10, true -> 1
  | 0 .. 15, _ -> 2
  | _, false -> 3
  | _, true -> 4
;;

test "exhaustive overlap 1" (f35 7) true 1;
test "exhaustive overlap 2" (f35 7) false 2;
test "exhaustive overlap 3" (f35 12) true 2;
test "exhaustive overlap 4" (f35 20) false 3;
test "exhaustive overlap 5" (f35 20) true 4;
()
;;

(* The same interval appearing twice with an overlapping interval in
   between: the duplicate key must not perturb dispatch order. *)

let f36 x y = match x, y with
  | 0 .. 10, true -> 1
  | 5 .. 15, _ -> 2
  | 0 .. 10, false -> 3
  | _ -> 4
;;

test "dup key 1" (f36 7) true 1;
test "dup key 2" (f36 7) false 2;
test "dup key 3" (f36 3) false 3;
test "dup key 4" (f36 12) true 2;
test "dup key 5" (f36 20) true 4;
()
;;

(* Single-column duplicate key with an intervening overlap; the guard
   keeps every row useful. *)

let f37 b x = match x with
  | 0 .. 10 when b -> 1
  | 5 .. 15 -> 2
  | 0 .. 10 -> 3
  | _ -> 0
;;

test "guard dup 1" (f37 true) 7 1;
test "guard dup 2" (f37 false) 7 2;
test "guard dup 3" (f37 false) 3 3;
test "guard dup 4" (f37 false) 12 2;
test "guard dup 5" (f37 true) 20 0;
()
;;

(* Boxed-int and wide-int variants of the multi-column overlap *)

let f38 x y = match x, y with
  | 0l .. 10l, true -> 1
  | 5l .. 15l, _ -> 2
  | _ -> 4
;;

test "int32 multi-col 1" (f38 7l) true 1;
test "int32 multi-col 2" (f38 7l) false 2;
test "int32 multi-col 3" (f38 12l) false 2;
test "int32 multi-col 4" (f38 20l) false 4;
()
;;

let f39 x y = match x, y with
  | 0 .. 1000, true -> 1
  | 500 .. 2000, _ -> 2
  | _ -> 4
;;

test "wide multi-col 1" (f39 600) true 1;
test "wide multi-col 2" (f39 600) false 2;
test "wide multi-col 3" (f39 1500) false 2;
test "wide multi-col 4" (f39 100) false 4;
()
;;

(* Intervals nested under constructors *)

let f40 x = match x with
  | Some (0 .. 10) -> 1
  | Some _ -> 2
  | None -> 3
;;

test "nested Some 1" f40 (Some 5) 1;
test "nested Some 2" f40 (Some 11) 2;
test "nested Some 3" f40 None 3;
()
;;

(* Interval bound to a variable with [as] *)

let f41 x = match x with
  | (0 .. 10 as y) -> y * 10
  | _ -> -1
;;

test "as-bound 1" f41 5 50;
test "as-bound 2" f41 0 0;
test "as-bound 3" f41 11 (-1);
()
;;

(* Exception rows mixed with interval rows *)

exception Interrupt

let f42 f = match f () with
  | 0 .. 10 -> 1
  | exception Interrupt -> 2
  | _ -> 3
;;

test "exception row 1" f42 (fun () -> 5) 1;
test "exception row 2" f42 (fun () -> raise Interrupt) 2;
test "exception row 3" f42 (fun () -> 42) 3;
()
;;

(* Hexadecimal and underscore literals as bounds *)

let f43 x = match x with
  | 0x0A .. 0xFF -> 1
  | 1_000 .. 2_000 -> 2
  | _ -> 0
;;

test "hex bound 1" f43 0x10 1;
test "hex bound 2" f43 0xFF 1;
test "underscore bound" f43 1_500 2;
test "hex bound 3" f43 5 0;
()
;;

(* Overlapping intervals in both columns *)

let f44 x y = match x, y with
  | 0 .. 10, 0 .. 10 -> 1
  | 5 .. 15, 5 .. 15 -> 2
  | _ -> 0
;;

test "ixi overlap 1" (f44 7) 3 1;
test "ixi overlap 2" (f44 7) 7 1;
test "ixi overlap 3" (f44 7) 12 2;
test "ixi overlap 4" (f44 12) 7 2;
test "ixi overlap 5" (f44 3) 12 0;
()
;;

(* Widths around the switch-expansion threshold (256 points) *)

let f45 x = match x with
  | 0 .. 254 -> 1      (* 255 points: expanded *)
  | 300 .. 555 -> 2    (* 256 points *)
  | 600 .. 856 -> 3    (* 257 points: bound checks *)
  | _ -> 0
;;

test "width 255" f45 254 1;
test "width 256" f45 555 2;
test "width 257" f45 856 3;
test "width gap" f45 255 0;
test "width gap 2" f45 599 0;
()
;;

(* Negative boxed bounds *)

let f46 x = match x with
  | -5l .. 5l -> 1
  | _ -> 0
;;

test "neg int32 1" f46 (-5l) 1;
test "neg int32 2" f46 (-3l) 1;
test "neg int32 3" f46 6l 0;
test "neg int32 4" f46 (-6l) 0;
()
;;

(* Intervals in record fields *)

type f47_rec = { a : int; b : bool }

let f47 x = match x with
  | { a = 0 .. 10; b = true } -> 1
  | { a = 5 .. 15; _ } -> 2
  | _ -> 3
;;

test "record field 1" f47 { a = 7; b = true } 1;
test "record field 2" f47 { a = 7; b = false } 2;
test "record field 3" f47 { a = 3; b = false } 3;
()
;;

(* Intervals under lazy *)

let f48 x = match x with
  | lazy (0 .. 10) -> 1
  | _ -> 2
;;

test "lazy 1" f48 (lazy 5) 1;
test "lazy 2" f48 (lazy 11) 2;
()
;;

(* Unspaced bounds: an integer literal immediately followed by ".."
   lexes as an interval bound, not as a float literal *)

let f49 x = match x with
  | 0..10 -> 1
  | 0x10..0x20 -> 2
  | 1_000..2_000 -> 3
  | _ -> 0
;;

test "unspaced 1" f49 5 1;
test "unspaced 2" f49 0x18 2;
test "unspaced 3" f49 1_500 3;
test "unspaced 4" f49 100 0;
()
;;

(* Unspaced signed bounds: a sign directly followed by a digit after
   ".." is an interval bound *)

let f50 x = match x with
  | 0..-5 -> 1
  | 1..+5 -> 2
  | _ -> 0
;;

test "signed unspaced 1" f50 (-3) 1;
test "signed unspaced 2" f50 0 1;
test "signed unspaced 3" f50 3 2;
test "signed unspaced 4" f50 6 0;
()
;;

(* Non-regression: a dot-operator applied to an unspaced float
   literal keeps its historical meaning, [2..%(i)] is [(2.).%(i)] *)

let ( .%() ) (x : float) (i : int) = x +. float_of_int i

let f51 i = 2..%(i)
;;

test "float dotop paren" f51 3 5.0;
()
;;

let ( .%[] ) (x : float) (i : int) = x *. float_of_int i

let f52 i = 2..%[i]
;;

test "float dotop bracket" f52 3 6.0;
()
;;

let f53 i =
  let ( .-() ) (x : float) (n : int) = x -. float_of_int n in
  2..-(i)
;;

test "float dotop minus" f53 3 (-1.0);
()
;;

(* TEST
 include testing;
*)
