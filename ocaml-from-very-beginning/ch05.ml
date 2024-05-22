
let rec sorted: 'a list -> bool = fun l ->
        match l with
        [] -> true
        | x::xs -> (match xs with [] -> true | y::_ -> x <= y) && (sorted xs);;

let sorted': 'a list -> bool = 
        let rec srt': 'a -> 'a list -> bool = fun v l ->
                match l with
                [] -> true
                | x::xs -> (v <= x) && (srt' x xs)
        in fun lst ->
                match lst with
                [] -> true
                | x::xs -> srt' x xs;;

(*
insertion sort
*)

let insert: 'a -> 'a list -> 'a list =
        let rec ins_helper v left_acc right_rem =
                match right_rem with
                  [] -> left_acc
                | x::xs -> if x < v then
                        ins_helper v (left_acc @ [x]) xs
                        else left_acc @ [v] @ right_rem
        in fun v l -> ins_helper v [] l;;

let rec isort l =
        match l with
        [] -> []
        | x::xs -> insert x (isort xs);;

(* merge sort *)
let merge: 'a list -> 'a list -> 'a list =
        let rec helper = fun accum x y ->
                match x, y with
                [], _ -> accum @ y
                | _, [] -> accum @ x
                | xi::xs, yi::_ when xi <= yi -> helper (accum @ [xi]) xs y
                | _, yi::ys -> helper (accum @ [yi]) x ys
        in fun x y -> helper [] x y;;

let split: 'a list -> 'a list * 'a list =
        let rec helper = fun a1 a2 rem ->
                match rem with
                [] -> a1, a2
                | [x] -> (a1 @ [x]), a2
                | x::y::xs -> helper (a1 @ [x]) (a2 @ [y]) xs
        in helper [] [];;


let rec msort: 'a list -> 'a list = fun l ->
        match l with
        | [] | [_] -> l
        | _ -> let x, y = split l in merge (msort x) (msort y);;

let rec splitq v lacc gacc rem =
        match rem with
        [] -> lacc , gacc
        | x::xs when x < v -> splitq v (lacc @ [x]) gacc xs
        | x:: xs -> splitq v lacc (gacc @ [x]) xs;;

let rec qsort l =
        match l with
        [] -> []
        | x::xs -> let l, r = splitq x [] [] xs in
                (qsort l) @ [x] @ (qsort r);;

(*
https://stackoverflow.com/questions/54188557/random-number-generation-in-ocaml
*)
let rec gen size l =
  if size = 0 then
    List.rev l
  else
    let n = Random.int 1000000 in
    let list = n :: l in
    gen (size - 1) list;;
