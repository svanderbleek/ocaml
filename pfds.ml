exception Empty

module type Stack = sig
  type 'a t

  val empty: 'a t
  val isEmpty: 'a t -> bool
  val cons: 'a t -> 'a -> 'a t
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
end

module ListStack: Stack = struct
  type 'a t = 'a list

  let empty = []
  let isEmpty s = s = []
  let cons s a = a :: s
  let head s =
    match s with
    | [] -> raise Empty
    | h :: _ -> h
  let tail s =
    match s with
    | [] -> raise Empty
    | _ :: t -> t
end

module CustomStack: Stack = struct
  type 'a t
    = Nil
    | Cons of 'a * 'a t

  let empty = Nil
  let isEmpty s = s = Nil
  let cons s a = Cons (a, s)
  let head s =
    match s with
    | Nil -> raise Empty
    | Cons (h, _) -> h
  let tail s =
    match s with
    | Nil -> raise Empty
    | Cons (_, t) -> t
end

let rec suffixes l =
  match l with
  | [] -> []
  | _ :: t -> l :: suffixes t

module type Set = sig
  type elem
  type set

  val empty: set
  val insert: set -> elem -> set
  val member: set -> elem -> bool
end

module type Ord = sig
  type t
  type ord = LT | EQ | GT

  val eq: t -> t -> ord
end

module UnbalancedSet (Elem: Ord): Set = struct
  type elem = Elem.t

  type tree
    = Empty
    | Tree of tree * elem * tree

  type set = tree

  let empty = Empty

  let rec member t a =
    match t with
    | Empty -> false
    | Tree (l, b, r) ->
      match Elem.eq a b with
      | Elem.LT -> member l a
      | Elem.EQ -> true
      | Elem.GT -> member r a

  let rec insert t a =
    match t with
    | Empty -> Tree (Empty, a, Empty)
    | Tree (l, b, r) as t ->
      match Elem.eq a b with
      | Elem.LT -> Tree (insert l a, b, r)
      | Elem.EQ -> t
      | Elem.GT -> Tree (r, b, insert r a)

  exception AbortCopy

  let insert_exn t a =
    let rec insert_ t a =
      match t with
      | Empty -> Tree (Empty, a, Empty)
      | Tree (l, b, r) ->
        match Elem.eq a b with
        | Elem.LT -> Tree (insert l a, b, r)
        | Elem.EQ -> raise AbortCopy
        | Elem.GT -> Tree (r, b, insert r a)
    in
    try insert_ t a with
    | AbortCopy -> t

  let rec complete i a =
    if i = 0
    then Tree (Empty, a, Empty)
    else Tree (complete (i-1) a, a, complete (i-1) a)
end

module type FiniteMap = sig
  type key
  type 'a map

  val empty: 'a map
  val bind: 'a map -> key -> 'a -> 'a map
  val lookup: 'a map -> key -> 'a
end

module UnbalancedFiniteMap (Key: Ord): FiniteMap = struct
  type key = Key.t
  type 'a map
    = Empty
    | Map of 'a map * (key * 'a) * 'a map

  let empty = Empty
  let bind m k a = raise Not_found
  let lookup m k = raise Not_found
end
