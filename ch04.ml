(*
function composition
*)
let (<|): ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f(g x);;

(*
in-book examples
*)
let isempty l = match l with [] -> true | _ -> false;;
let rec len l = match l with [] -> 0 | x::xs -> 1 + len xs;;
let rec sum l = match l with [] -> 0 | x::xs -> x + sum xs;;

(* sum and rec are pretty similar; let us try to express len in terms of sum *)
let rec maplist f l = match l with [] -> [] | x::xs -> (f x) :: maplist f xs;;
let len' l = maplist (fun _ -> 1) l |> sum;;

let rec odds l = match l with x::_::xs -> x :: (odds xs) | _ -> l;;

(* append list *)
let rec app l v = match l with [] -> [v] | x::xs -> x::(app xs v);;
(* reverse list *)
let rec rev l = match l with x::xs -> app (rev xs) x | [] -> [];;

(* tail-recursively *)
let rec rev_helper: 'a list -> 'a list -> 'a list = fun acc lst ->
        match lst with
          []    -> acc
        | x::xs -> rev_helper ([x] @ acc) xs;;

let rev' = rev_helper [];;

(* taking/dropping first n elems *)
let rec take n l = if n <= 0 then [] else
        match l with x::xs -> x::(take (n-1) xs) | _ -> [];;
let rec drop n l = if n <= 0 then l else
        match l with x::xs -> drop (n-1) xs | _ -> [];;


(*
exercises
*)
let rec even: ('a list -> 'a list) = fun l ->
        match l with _::x::xs -> x :: (even xs) | _ -> [];;

let rec cttrue: (bool list -> int) = fun l ->
        match l with
        [] -> 0 | x::xs -> if x then 1 else 0 + cttrue xs;;

let rec cttrue_tailrec n l = match l with
          [] -> n
        | x::xs -> cttrue_tailrec (n + if x then 1 else 0) xs;;

let cttrue'= cttrue_tailrec 0;;

let rec filter : (('a -> bool) -> 'a list -> 'a list) = fun f l ->
        match l with
          [] -> []
        | x::xs when (f x) -> x::(filter f xs)
        | _::xs -> filter f xs;;


let cttrue'' l = filter (fun x -> x) l |> len;;
let ctfalse l = filter (fun x -> not x) l |> len;;


let rec mkpalindrome: 'a list -> 'a list = fun l -> match l with
        [] -> [] | x::xs -> [x] @ (mkpalindrome xs) @ [x];;

let rec mkpalindrome': 'a list -> 'a list = fun l -> l @ (rev' l);;

let rec ispalindrome (l: 'a list): bool = l = (rev l);;

let rec droplast l = match l with
        [] | [_] -> []
        | x::xs -> x::(droplast xs);;

let rec droplast_tailrec accum l =
        match l with
        [] | [_] -> accum
        | x::xs -> droplast_tailrec (accum @ [x]) xs;;

let droplast' = droplast_tailrec [];;

let droplast'': 'a list -> 'a list = rev' <| drop 1 <| rev';;

let rec member v l = match l with
        [] -> false | x::xs -> (x = v) || (member v xs);;


let rec mkset_help: 'a list -> 'a list -> 'a list = fun acc l ->
        match l with
         [] -> acc
        | x::xs -> let acc' = if (member x acc) then acc else x::acc in
        mkset_help acc' xs;;

let mkset = mkset_help [];;
