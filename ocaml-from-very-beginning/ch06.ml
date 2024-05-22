let rec map : ('a -> 'b) -> 'a list -> 'b list =
 fun f l -> match l with [] -> [] | x :: xs -> f x :: map f xs

let merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list =
  let rec helper cmp accum x y =
    match (x, y) with
    | [], _ -> accum @ y
    | _, [] -> accum @ x
    | xi :: xs, yi :: _ when cmp xi yi -> helper cmp (accum @ [ xi ]) xs y
    | _, yi :: ys -> helper cmp (accum @ [ yi ]) x ys
  in
  fun cmp x y -> helper cmp [] x y

let split : 'a list -> 'a list * 'a list =
  let rec helper a1 a2 rem =
    match rem with
    | [] -> (a1, a2)
    | [ x ] -> (a1 @ [ x ], a2)
    | x :: y :: xs -> helper (a1 @ [ x ]) (a2 @ [ y ]) xs
  in
  helper [] []

let rec msort : ('a -> 'a -> bool) -> 'a list -> 'a list =
 fun cmp l ->
  match l with
  | [] | [ _ ] -> l
  | _ ->
      let x, y = split l in
      merge cmp (msort cmp x) (msort cmp y)

(*
exercises
*)
let calm : char list -> char list =
  let f : char -> char = fun x -> if x = '!' then '.' else x in
  map f

let clip : int list -> int list =
  let f : int -> int -> int -> int =
   fun lower upper value ->
    if value < lower then lower else if value > upper then upper else value
  in
  map (f 0 100)

let rec apply : ('a -> 'a) -> int -> 'a -> 'a =
 fun f n v -> if n = 1 then f v else f (apply f (n - 1) v)

let insert : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list =
  let rec ins_helper cmp v left_acc right_rem =
    match right_rem with
    | [] -> left_acc
    | x :: xs ->
        if cmp x v then ins_helper cmp v (left_acc @ [ x ]) xs
        else left_acc @ [ v ] @ right_rem
  in
  fun cmp v l -> ins_helper cmp v [] l

let rec isort : ('a -> 'a -> bool) -> 'a list -> 'a list =
 fun cmp l -> match l with [] -> [] | x :: xs -> insert cmp x (isort cmp xs)

let rec splitq :
    ('a -> 'a -> bool) ->
    'a ->
    'a list ->
    'a list ->
    'a list ->
    'a list * 'a list =
 fun cmp v lacc gacc rem ->
  match rem with
  | [] -> (lacc, gacc)
  | x :: xs when cmp x v -> splitq cmp v (lacc @ [ x ]) gacc xs
  | x :: xs -> splitq cmp v lacc (gacc @ [ x ]) xs

let rec qsort : ('a -> 'a -> bool) -> 'a list -> 'a list =
 fun cmp l ->
  match l with
  | [] -> []
  | x :: xs ->
      let l, r = splitq cmp x [] [] xs in
      qsort cmp l @ [ x ] @ qsort cmp r

(*
https://stackoverflow.com/questions/54188557/random-number-generation-in-ocaml
*)
let rec gen size l =
  if size = 0 then List.rev l
  else
    let n = Random.int 1000000 in
    let list = n :: l in
    gen (size - 1) list

let filter : ('a -> bool) -> 'a list -> 'a list =
  let rec helper : ('a -> bool) -> 'a list -> 'a list -> 'a list =
   fun f acc l ->
    match l with
    | [] -> acc
    | x :: xs when f x -> helper f (acc @ [ x ]) xs
    | x :: xs -> helper f acc xs
  in
  fun f l -> helper f [] l

let rec for_all : ('a -> bool) -> 'a list -> bool =
 fun f l -> match l with [] -> true | x :: xs -> f x && for_all f xs

let mapl : ('a -> 'b) -> 'a list list -> 'b list list = fun f -> map (map f)
