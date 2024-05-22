let fs : 'a * 'b -> 'a = fun l -> match l with x, _ -> x
let sn : 'a * 'b -> 'b = fun l -> match l with _, x -> x

(*
a naive dictionary can be stored as a value of type ('a * 'b) list.
*)

let rec lookup : 'a -> ('a * 'b) list -> ('a * 'b) option =
 fun k dict ->
  match dict with
  | [] -> None
  | (x, v) :: _ when k = x -> Some (k, v)
  | _ :: xs -> lookup k xs

let insert_or_replace : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list =
  let rec helper accum key value rem =
    match rem with
    | [] -> accum @ [ (key, value) ]
    | (k, v) :: xs when k = key -> accum @ [ (key, value) ] @ xs
    | x :: xs -> helper (accum @ [ x ]) key value xs
  in
  fun k v r -> helper [] k v r

let add : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list option =
  let rec helper accum key value rem =
    match rem with
    | [] -> Some (accum @ [ (key, value) ])
    | (k, v) :: xs when k = key -> None
    | x :: xs -> helper (accum @ [ x ]) key value xs
  in
  fun k v r -> helper [] k v r

let exists_key : 'a -> ('a * 'b) list -> bool = fun k l -> None <> lookup k l

(*
exercises
*)
let num_keys : ('a * 'b) list -> int = List.length

let comb_lists : 'a list -> 'b list -> ('a * 'b) list option =
  let rec helper accum a b =
    match (a, b) with
    | [], [] -> Some accum
    | _, [] | [], _ -> None (* unequal lengths *)
    | x :: xs, y :: ys -> helper (accum @ [ (x, y) ]) xs ys
  in
  fun a b -> helper [] a b

let split_dict : ('a * 'b) list -> 'a list * 'b list =
  let rec helper ak av rem =
    match rem with
    | [] -> (ak, av)
    | (k, v) :: xs -> helper (ak @ [ k ]) (av @ [ v ]) xs
  in
  fun lst -> helper [] [] lst

let try_add : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list =
 fun dict k v -> match add k v dict with Some v -> v | None -> dict

let rec union : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list =
 fun l r -> match r with [] -> l | (k, v) :: xs -> union (try_add l k v) xs

let mk_dict : ('a * 'b) list -> ('a * 'b) list = fun l -> union [] l
