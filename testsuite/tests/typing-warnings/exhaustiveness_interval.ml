(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
*)

(* Non-exhaustive integer interval *)
let f (x : int) = match x with
  | 0 .. 10 -> 1
  | 11 .. 20 -> 2;;
[%%expect {|
Lines 1-3, characters 18-17:
1 | ..................match x with
2 |   | 0 .. 10 -> 1
3 |   | 11 .. 20 -> 2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "21"
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Redundant constant inside interval *)
let g x = match x with
  | 0 .. 10 -> 1
  | 5 -> 2
  | _ -> 3;;
[%%expect {|
Line 3, characters 4-5:
3 |   | 5 -> 2
        ^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Redundant sub-interval *)
let h x = match x with
  | 0 .. 10 -> 1
  | 3 .. 7 -> 2
  | _ -> 3;;
[%%expect {|
Line 3, characters 4-10:
3 |   | 3 .. 7 -> 2
        ^^^^^^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Exhaustive char interval *)
let i (c : char) = match c with
  | '\000' .. '\255' -> 1;;
[%%expect {|
val i : char -> int = <fun>
|}]

(* Float interval rejected *)
let j x = match x with
  | 1.0 .. 2.0 -> 1
  | _ -> 2;;
[%%expect {|
Line 2, characters 4-7:
2 |   | 1.0 .. 2.0 -> 1
        ^^^
Error: Only character and integer intervals are supported in patterns.
|}]

(* String interval rejected *)
let k x = match x with
  | "a" .. "z" -> 1
  | _ -> 2;;
[%%expect {|
Line 2, characters 4-7:
2 |   | "a" .. "z" -> 1
        ^^^
Error: Only character and integer intervals are supported in patterns.
|}]

(* GADT with interval counter-example *)
type _ ty = Int : int ty | Bool : bool ty

let l (type a) (t : a ty) (x : a) = match t, x with
  | Int, 0 .. 10 -> 1
  | Bool, true -> 2
  | Bool, false -> 3;;
[%%expect {|
type _ ty = Int : int ty | Bool : bool ty
Lines 3-6, characters 36-20:
3 | ....................................match t, x with
4 |   | Int, 0 .. 10 -> 1
5 |   | Bool, true -> 2
6 |   | Bool, false -> 3..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(Int, 11)"
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Int32 non-exhaustive *)
let m (x : int32) = match x with
  | 0l .. 100l -> 1
  | 101l .. 200l -> 2;;
[%%expect {|
Lines 1-3, characters 20-21:
1 | ....................match x with
2 |   | 0l .. 100l -> 1
3 |   | 101l .. 200l -> 2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "201l"
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Int64 redundant sub-interval *)
let n (x : int64) = match x with
  | 0L .. 100L -> 1
  | 50L .. 80L -> 2
  | _ -> 3;;
[%%expect {|
Line 3, characters 4-14:
3 |   | 50L .. 80L -> 2
        ^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Nativeint non-exhaustive *)
let o (x : nativeint) = match x with
  | 0n .. 50n -> 1
  | 51n .. 100n -> 2;;
[%%expect {|
Lines 1-3, characters 24-20:
1 | ........................match x with
2 |   | 0n .. 50n -> 1
3 |   | 51n .. 100n -> 2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "101n"
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Nativeint exhaustive with wildcard *)
let p (x : nativeint) = match x with
  | 0n .. 100n -> 1
  | _ -> 2;;
[%%expect {|
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Gap between intervals: counter-example is the gap value *)
let q x = match x with
  | 0 .. 5 -> 1
  | 7 .. 10 -> 2;;
[%%expect {|
Lines 1-3, characters 10-16:
1 | ..........match x with
2 |   | 0 .. 5 -> 1
3 |   | 7 .. 10 -> 2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "6"
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Char gap: counter-example shows the gap character *)
let r (c : char) = match c with
  | '\000' .. 'a' -> 1
  | 'c' .. '\255' -> 2;;
[%%expect {|
Lines 1-3, characters 19-22:
1 | ...................match c with
2 |   | '\000' .. 'a' -> 1
3 |   | 'c' .. '\255' -> 2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "'b'"

val r : char -> int = <fun>
|}]

(* Overlapping intervals with wildcard: no warning *)
let s x = match x with
  | 0 .. 10 -> 1
  | 5 .. 20 -> 2
  | _ -> 3;;
[%%expect {|
Line 3, characters 4-11:
3 |   | 5 .. 20 -> 2
        ^^^^^^^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Wildcard before interval makes trailing interval redundant *)
let t x = match x with
  | 0 .. 10 -> 1
  | _ -> 2
  | 11 .. 20 -> 3;;
[%%expect {|
Line 4, characters 4-12:
4 |   | 11 .. 20 -> 3;;
        ^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Overlapping intervals: second is partially redundant but not unused *)
let u x = match x with
  | 0 .. 5 -> 1
  | 5 .. 10 -> 2
  | _ -> 3;;
[%%expect {|
Line 3, characters 4-11:
3 |   | 5 .. 10 -> 2
        ^^^^^^^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* Mismatched interval bound types *)
let v x = match x with
  | 0l .. 10 -> 1
  | _ -> 2;;
[%%expect {|
Line 2, characters 10-12:
2 |   | 0l .. 10 -> 1
              ^^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "int32"
|}]

let w x = match x with
  | 0 .. 10L -> 1
  | _ -> 2;;
[%%expect {|
Line 2, characters 9-12:
2 |   | 0 .. 10L -> 1
             ^^^
Error: This pattern matches values of type "int64"
       but a pattern was expected which matches values of type "int"
|}]

let y x = match x with
  | 'a' .. 10 -> 1
  | _ -> 2;;
[%%expect {|
Line 2, characters 11-13:
2 |   | 'a' .. 10 -> 1
               ^^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "char"
|}]

(* Full-domain coverage: both int32 arms together cover the whole
   type, so no partial-match warning is expected once exhaustiveness
   is interval-aware *)
let full_range32 (x : int32) = match x with
  | -2147483648l .. 0l -> 1
  | 1l .. 2147483647l -> 2;;
[%%expect {|
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]

(* An arm covered by the union of two earlier intervals is redundant;
   detecting it requires splitting overlapping intervals *)
let union_covered x = match x with
  | 0 .. 10 -> 1
  | 11 .. 20 -> 2
  | 5 .. 15 -> 3
  | _ -> 0;;
[%%expect {|
Line 4, characters 4-11:
4 |   | 5 .. 15 -> 3
        ^^^^^^^
Warning 11 [redundant-case]: this match case is unused.
>> Fatal error: Matching.do_compile_matching: Interval
Uncaught exception: Misc.Fatal_error

|}]
