type 'a tree =
  | Br of 'a * 'a tree * 'a tree
  | Lf

let rec size t =
  match t with
  | Lf -> 0
  | Br (_, l, r) -> 1 + size l + size r

and depth t =
  match t with
  | Lf -> 0
  | Br (_, l, r) -> 1 + max (depth l) (depth r)

and list_from_t t =
  match t with
  | Lf -> []
  | Br (a, l, r) -> [a] @ list_from_t l @ list_from_t r

and lookup t k =
  match t with
  | Lf -> None
  | Br ((k', v), l, r) ->
    if k = k' then Some v
    else if k < k' then lookup l k
    else lookup r k

and insert t k v =
  match t with
  | Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v') as b, l, r) ->
    if k = k' then Br ((k, v), l, r)
    else if k < k' then Br (b, insert l k v, r)
    else Br (b, l, insert r k v)

and member t a =
  match t with
  | Lf -> false
  | Br (b, l, r) ->
    a = b || member l a || member r a

and flip t =
  match t with
  | Br (a, l, r) -> Br (a, flip r, flip l)
  | _ -> t

and same_shape a b =
  match a, b with
  | Lf, Lf -> true
  | Br (_, la, ra), Br (_, lb, rb) -> same_shape la lb && same_shape ra rb
  | _, _ -> false

type 'a rose =
  | Br of 'a * 'a rose list

let rec rsize (Br (_, l)) =
  List.fold_left (+) 1 (List.map rsize l)

let rec read_d () =
  try
    let i = read_int () in
    if i = 0 then [] else
    let n = read_line () in
    (i, n) :: read_d ()
  with
  | Failure m ->
    print_string m;
    print_newline ();
    read_d ()
