
(*
the function cannot be called in that way because it would result in
2/10, 2/20, etc. (the first argument to / is the divisor)
*)
let half_all: int list -> int list = (fun x -> x / 2) |> List.map;;

let mapll: ('a -> 'b) -> 'a list list list -> 'b list list list =
        fun f -> f |> List.map |> List.map |> List.map;;

let rec truncate_single acc n l =
        if n <= 0 then acc else
        match l with
        | [] -> acc 
        | x::xs -> truncate_single (acc @ [x]) (n-1) xs;;

let truncate:  int -> 'a list list -> 'a list list = fun n l ->
        l |> (truncate_single [] n |> List.map);;

let fstl default l = match l with [] -> default | x::_ -> x;;

let get_all_firsts: int -> int list list -> int list =
        fun default lists -> List.map (fstl default) lists;;
