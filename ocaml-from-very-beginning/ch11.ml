type comparison = Lt | Eq | Gt

let cmp : 'a -> 'a -> comparison =
 fun x y -> if x < y then Lt else if x = y then Eq else Gt

let clsc : 'a option -> 'a -> 'a =
 fun x y -> match x with None -> y | Some q -> q

type 'a tree =
  (*  Leaf  *)
  | Lf
  (* Branch *)
  | Br of 'a * 'a tree * 'a tree

let rec height : 'a tree -> int =
 fun t ->
  match t with Lf -> 0 | Br (_, l, r) -> 1 + Int.max (height l) (height r)

let rec total : 'a tree -> int =
 fun t -> match t with Lf -> 0 | Br (_, l, r) -> 1 + total l + total r

type traversal = Preorder | Inorder | Postorder

let rec list_of_tree : traversal -> 'a tree -> 'a list =
 fun k tr ->
  match tr with
  | Lf -> []
  | Br (v, lc, rc) -> (
      let pre = list_of_tree k lc in
      let pos = list_of_tree k rc in
      let cur = [ v ] in
      match k with
      | Preorder -> cur @ pre @ pos
      | Inorder -> pre @ cur @ pos
      | Postorder -> pos @ cur @ pre)

(* binary search trees !! *)
let rec lookup : 'a -> ('a * 'b) tree -> 'b option =
 fun k tree ->
  match tree with
  | Lf -> None
  | Br ((k', v), left, right) -> (
      match cmp k k' with
      | Lt -> lookup k left
      | Eq -> Some v
      | Gt -> lookup k right)

let insert : 'a -> 'b -> ('a * 'b) tree -> ('a * 'b) tree option =
  let rec helper k v tree =
    match tree with
    | Lf -> Br ((k, v), Lf, Lf)
    | Br ((k', v'), left, right) -> (
        match cmp k k' with
        | Lt -> Br ((k', v'), helper k v left, right)
        | Gt -> Br ((k', v'), left, helper k v right)
        | Eq ->
            raise
              (Invalid_argument
                 "already\n                                exists"))
  in
  fun k v t -> try Some (helper k v t) with Invalid_argument _ -> None

let insert' : 'a -> 'a tree -> 'a tree option =
  let rec helper v tr =
    match tr with
    | Lf -> Br (v, Lf, Lf)
    | Br (v', l, r) -> (
        match cmp v v' with
        | Lt -> Br (v', helper v l, r)
        | Gt -> Br (v', l, helper v r)
        | Eq ->
            raise
              (Invalid_argument
                 "already\n                                exists"))
  in
  fun v t -> try Some (helper v t) with Invalid_argument _ -> None

let rec contains : 'a -> 'a tree -> bool =
 fun k tree ->
  match tree with
  | Lf -> false
  | Br (k', l, r) -> k' = k || contains k l || contains k r

let rec flip : 'a tree -> 'a tree = function
  | Lf -> Lf
  | Br (k, l, r) -> Br (k, flip r, flip l)

let rec same_shape : 'a tree -> 'b tree -> bool =
 fun a b ->
  match (a, b) with
  | Lf, Lf -> true
  | Br (_, l, r), Br (_, l', r') -> same_shape l l' && same_shape r r'
  | _ -> false

let tree_of_list : 'a list -> 'a tree =
  let rec helper : 'a tree -> 'a list -> 'a tree =
   fun accum l ->
    match l with
    | [] -> accum
    | x :: xs ->
        let acc' = clsc (insert' x accum) accum in
        helper acc' xs
  in
  fun l -> helper Lf l

let rec union : ('a * 'b) tree -> ('a * 'b) tree -> ('a * 'b) tree =
 fun accum rst ->
  match rst with
  | Lf -> accum
  | Br ((k, v), l, r) ->
      let acc' = clsc (insert k v accum) accum in
      union (union acc' l) r

type 'a multitree =
  (*  Leaf  *)
  | Lf
  (* Branch *)
  | Br of 'a * 'a multitree list

let rec total' : 'a multitree -> int = function
  | Lf -> 0
  | Br (_, lst) -> 1 + (lst |> List.map total' |> List.fold_left ( + ) 0)

let rec height' : 'a multitree -> int = function
  | Lf -> 0
  | Br (_, lst) -> 1 + (lst |> List.map height' |> List.fold_left Int.max 0)

let rec map' : ('a -> 'b) -> 'a multitree -> 'b multitree =
 fun f x ->
  match x with Lf -> Lf | Br (value, l) -> Br (f value, List.map (map' f) l)
