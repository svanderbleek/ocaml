let rec fold_l f a l =
  match l with
  | [] -> a
  | h :: t ->
    fold_l f (f a h) t

let rec fold_r f l b =
  match l with
  | [] -> b
  | h :: t ->
    f h (fold_r f t b)

let mapr f l =
  fold_r (fun h b -> f h :: b) l []

let mk_set =
  fold_l (fun a h -> if List.mem h a then a else h :: a) []

let all =
  fold_l (&&) true

let any =
  fold_l (||) false

let flip f =
  fun x y -> f y x

let fold_r' f l b =
  fold_l (flip f) b (List.rev l)

let split l =
  fold_r
    (fun (x, y) (bx, by) -> (x::bx, x::by))
    l
    ([], [])

type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree

let rec fold_t f e t =
  match t with
  | Lf -> e
  | Br (a, l, r) ->
    f a (fold_t f e l) (fold_t f e r)

let size_t =
  fold_t (fun _ l r -> 1 + l + r) 0

let sum_t =
  fold_t (fun a l r -> a + l + r) 0

let pr_t =
  fold_t (fun a l r -> [a] @ l @ r) []

let in_t =
  fold_t (fun a l r -> l @ [a] @ r) []

let po_t =
  fold_t (fun a l r -> l @ r @ [a]) []

let balance expenses budget =
  fold_l (-) budget expenses

let len l =
  fold_l (fun a _ -> a + 1) 0 l

let last l =
  fold_l (fun _ h -> Some h) None l

let rev l =
  fold_l (fun a h -> h :: a) [] l

let mem x l =
  fold_l (fun a h -> a || h = x) false l

let spaces words =
  fold_l
    (fun a h -> if String.length a > 0 then a ^ " " ^ h else a ^ h)
    ""
    words

let depth t =
  fold_t (fun _ l r -> 1 + max l r) 0 t

let compare_time f a =
  let s = Unix.gettimeofday () in
  f a;
  let e = Unix.gettimeofday () in
  e -. s

let compare_times f g a =
  print_string "f time: ";
  let f_t = compare_time f a in
  print_float f_t;
  print_newline ();
  print_string "g time: ";
  let g_t = compare_time g a in
  print_float g_t;
  print_newline ();
  print_string "winner: ";
  if f_t < g_t then print_string "f" else print_string "g";
  print_newline ()

