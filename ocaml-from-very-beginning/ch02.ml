(*

examples from book

*)

(* recursive factorial *)
let rec fact n = if n < 2 then n else n * fact (n - 1)

(* euclidean gcd *)
let rec gcd x y = if y = 0 then x else gcd y (x mod y)

(*

exercises

*)

(* multen: int -> int *)
let multen x = 10 * x

(* myand : bool -> bool -> bool *)
let myand x y = if x then if y then true else false else false

(* power: int -> int -> int *)
let rec power x n = if n < 1 then 1 else x * power x (n - 1)

(* f: char -> bool (for f in isvowel, isconsonant) *)

let isvowel x = x = 'a' || x = 'e' || x = 'i' || x = 'o' || x = 'u'
let isconsonant x = not (isvowel x)

(*
let x = 1 in let x = 2 in x + x
This evaluates to 4.
*)

(*
Can you suggest a way of preventing the non-termination of the factorial
function in the case of a zero or negative argument?

- Dependent typing (positive integer only)
- Precondition of some sort
*)
