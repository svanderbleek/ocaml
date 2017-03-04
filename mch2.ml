type 'a lazylist =
  | Cons of 'a * (unit -> 'a lazylist)

let rec lseq n =
  Cons (n, fun () -> lseq (n + 1))

let lhd (Cons (n, _)) =
  n

let ltl (Cons (_, t)) =
  t ()

let rec ltake (Cons (h, t)) n =
  match n with
  | 0 -> []
  | _ -> h :: ltake (t ()) (n - 1)

let rec ldrop ((Cons (h, t)) as l) n =
  match n with
  | 0 -> l
  | _ -> ldrop (t ()) (n - 1)

let rec lmap f (Cons (h, t)) =
  Cons (f h, fun () -> lmap f (t ()))

let rec lfil f (Cons (h, t)) =
  if f h then
    Cons (h, fun () -> lfil f (t ()))
  else
    lfil f (t ())

let cubes =
  lfil
    (fun n -> n mod 5 = 0)
    (lmap (fun n -> n * n * n) (lseq 1))

let rec mkprimes (Cons (p, s)) =
  Cons (p, fun () ->
    mkprimes (lfil (fun n -> n mod p <> 0) (s ())))

let rec interleave (Cons (h, t)) s =
  Cons (h, fun () -> interleave s (t ()))

let rec lconst n =
  Cons (n, fun () -> lconst n)

let rec binary_lists l =
  Cons (l, fun () ->
    interleave (binary_lists (0 :: l)) (binary_lists (1 :: l)))

let rec mkevens n =
  Cons (n, fun () -> mkevens (n * 2))

let rec nth (Cons (h, t)) n =
  if n = 0
  then h
  else nth (t ()) (n-1)

let rec repeat_ l l' =
  match l' with
  | [] -> raise (Invalid_argument "empty list")
  | [x] -> Cons (x, fun () -> repeat_ l l)
  | h :: t -> Cons (h, fun () -> repeat_ l t)

let repeat l =
  repeat_ l l

let rec mkfib n m =
  Cons (n, fun () ->
  Cons (m, fun () ->
    mkfib (n+m) (n+m+m)))

let fib =
  mkfib 0 1

let rec skip (Cons (_, t)) =
  let Cons (n, ns) = t () in
  Cons (n, fun () -> skip (ns ()))

let unleave (Cons (x, xs)) =
  let Cons (y, ys) = xs () in
  (Cons (x, fun () -> skip (xs ()))
  ,Cons (y, fun () -> skip (ys ())))

let alphabase =
  int_of_char 'A'

let mkalpha n =
  let c = char_of_int (alphabase + n) in
  String.make 1 c

let rec alpha n =
  let c = mkalpha (n mod 26) in
  if n < 26
  then c
  else alpha (n / 26 - 1) ^ c

let alphas =
  lmap alpha (lseq 0)
