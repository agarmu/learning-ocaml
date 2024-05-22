type color = Red | Green | Blue | Yellow

let components : color -> int * int * int =
 fun c ->
  match c with
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)

type 'a sequence = Nil | Cons of 'a * 'a sequence

let rec length : 'a sequence -> int =
 fun l -> match l with Nil -> 0 | Cons (_, xs) -> 1 + length xs

let rec append : 'a -> 'a sequence -> 'a sequence =
 fun a l ->
  match l with Nil -> Cons (a, Nil) | Cons (x, xs) -> Cons (x, append a xs)

(*
type expr =
        | Num of int
        | Add of expr * expr
        | Sub of expr * expr
        | Mul of expr * expr
        | Div of expr * expr;;

let rec eval: expr -> int = fun e -> match e with
        | Num x -> x
        | Add (a, b) -> (eval a) + (eval b) 
        | Sub (a, b) -> (eval a) - (eval b)
        | Mul (a, b) -> (eval a) * (eval b)
        | Div (a, b) -> (eval a) / (eval b);;
*)
type rect = Square of int | Nonsquare of int * int

let mk_rect : int -> int -> rect =
 fun x y -> if x = y then Square x else Nonsquare (x, y)

let perim : rect -> int =
 fun r -> match r with Square l -> 4 * l | Nonsquare (x, y) -> 2 * (x + y)

let area : rect -> int =
 fun r -> match r with Square l -> l * l | Nonsquare (x, y) -> x * y

let rotate : rect -> rect =
 fun r -> match r with Nonsquare (w, h) -> Nonsquare (Int.max w h, w) | _ -> r

let rec take : int -> 'a sequence -> 'a sequence =
 fun n l ->
  match l with
  | _ when n <= 0 -> Nil
  | Nil -> Nil
  | Cons (x, xs) -> Cons (x, take (n - 1) xs)

let rec drop : int -> 'a sequence -> 'a sequence =
 fun n l ->
  match l with
  | _ when n <= 0 -> l
  | Nil -> Nil
  | Cons (x, xs) -> drop (n - 1) xs

let rec map : ('a -> 'b) -> 'a sequence -> 'b sequence =
 fun f l -> match l with Nil -> Nil | Cons (x, xs) -> Cons (f x, map f xs)

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

let rec pow : int -> int -> int =
 fun base e ->
  match e with
  | _ when e < 0 -> Invalid_argument "negative power" |> raise
  | 0 -> 1
  | 1 -> base
  | b ->
      let b = pow base (e / 2) in
      let q = if e mod 2 = 0 then 1 else base in
      b * b * q

let rec eval : expr -> int option =
  let rec h e =
    match e with
    | Num x -> x
    | Add (a, b) -> h a + h b
    | Sub (a, b) -> h a - h b
    | Mul (a, b) -> h a * h b
    | Div (a, b) -> h a / h b
    | Pow (a, b) -> pow (h a) (h b)
  in
  fun x -> try Some (h x) with Division_by_zero | Invalid_argument _ -> None
