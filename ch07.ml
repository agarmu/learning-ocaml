let smallest: 'a list -> 'a =
        let rec h: 'a -> 'a list -> 'a = fun cur l ->
                match l with [] -> cur
                | x::xs -> h (if x < cur then x else cur) xs
        in fun l ->
                match l with
                  [] -> raise Not_found
                | x::xs -> h x xs;;

let smallest_or_zero: int list -> int = fun l -> try smallest l with _ -> 0;;

exception NegativeNotAllowed;;

let smallest_num_smaller_than_sqrt: int -> int = 
        let rec guess_sm_sqrt: int -> int -> int -> int =
                fun v l u ->
                        let () = Printf.printf "[%d, %d]\n" l u in
                        if l = u then l else
                        let mid = (l + u + 1) / 2 in
                        let guess = mid * mid in 
                        if guess <= v then
                                guess_sm_sqrt v mid u
                        else
                                guess_sm_sqrt v l (mid-1)

        in fun x -> match x with
          _ when x < 0 -> raise NegativeNotAllowed
          | 1 | 2 | 3 -> 1
          | _ -> guess_sm_sqrt x 0 (x/2);;

(*
exceptions are inherently bad. algebraic data type better.
*)
