(*
examples from book
*)
let rec fact n = if n < 2 then n else n * fact (n - 1)
let rec fact' n = match n with 1 -> 1 | _ -> n * fact' (n - 1)
let rec gcd x y = if x = 0 then y else gcd (y mod x) x
let rec gcd' x y = match x with 0 -> y | _ -> gcd' (y mod x) x

(*
exercises
*)
let not' : bool -> bool = function x -> x = false
let not'' x = match x with true -> false | false -> true

let rec power x n =
  match n with
  | _ when n < 0 -> None
  | 0 -> Some 1
  | _ -> Option.map (( * ) x) (power x (n - 1))

let islower c = match c with 'a' .. 'z' -> true | _ -> false
let isupper c = match c with 'A' .. 'Z' -> true | _ -> false
