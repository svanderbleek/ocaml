open OUnit2

let rec last l =
  match l with
  | [] -> None
  | [x] -> Some x
  | h::t -> last t

let test_last _ =
  assert_equal ~msg:"finds last"
    (last ["a"; "b"; "c"; "d"]) (Some "d");
  assert_equal ~msg:"handles empty"
    (last []) None

let rec last_two l =
  match l with
  | [] -> None
  | [x] -> None
  | [x;y] -> Some (x,y)
  | h::t -> last_two t

let test_last_two _ =
  assert_equal ~msg:"finds last two"
    (last_two ["a"; "b"; "c"; "d"]) (Some ("c","d"));
  assert_equal ~msg:"handles empty"
    (last_two []) None;
  assert_equal ~msg:"handles single"
    (last_two ["a"]) None

let rec at n l =
  match l with
  | [] -> None
  | h::t ->
    match n with
    | 1 -> Some h
    | _ -> at (n-1) t

let test_at _ =
  assert_equal ~msg:"finds last"
    (at 3 ["a"; "b"; "c"; "d"]) (Some "c");
  assert_equal ~msg:"handles not enough"
    (at 3 ["a"]) (None)

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten_ n l =
  match n with
  | One a -> l @ [a]
  | Many [] -> l
  | Many (One a::t) -> flatten_ (Many t) (l @ [a])
  | Many (Many m::t) -> (flatten_ (Many m) l) @ (flatten_ (Many t) [])

let flatten n =
  flatten_ n []

let test_flatten _ =
  let node = Many [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]] in
  assert_equal ~msg:"flattens node"
    (flatten node) ["a"; "b"; "c"; "d"; "e"]

let rec compress l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::y::t ->
    if x = y
    then compress (x::t)
    else x::compress (y::t)

let test_compress _ =
  let i = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  let o = ["a"; "b"; "c"; "a"; "d"; "e"] in
  assert_equal ~msg:"eleminates dups"
    (compress i) o

let rec pack_ l a s =
  match l with
  | [] -> a
  | [x] -> a @ [x::s]
  | x::y::t ->
    let s' = x::s in
    if x = y
    then pack_ (y::t) a s'
    else pack_ (y::t) (a @ [s']) []

let pack l =
  pack_ l [] []

let test_pack _ =
  let i = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] in
  let o = [["a";"a";"a";"a"];["b"];["c";"c"];["a";"a"];["d";"d"];["e";"e";"e";"e"]] in
  assert_equal ~msg:"collect dups"
    (pack i) o

let rec encode_ l n =
  match l with
  | [] -> []
  | [x] -> [(n,x)]
  | x::y::t ->
    if x = y
    then encode_ (x::t) (n+1)
    else (n,x)::encode_ (y::t) 1

let encode l =
  encode_ l 1

let test_encode _ =
  let i = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  let o = [(4,"a"); (1,"b"); (2,"c"); (2,"a"); (1,"d"); (4,"e")] in
  assert_equal ~msg:"encodes runs"
    (encode i) o

type 'a rle =
  | One of 'a
  | Many of int * 'a

let mkrle x n =
  if n = 1
  then One x
  else Many (n,x)

let rec encode_' l n =
  match l with
  | [] -> []
  | [x] -> [mkrle x n]
  | x::y::t ->
    if x = y
    then encode_' (x::t) (n+1)
    else (mkrle x n)::encode_' (y::t) 1

let encode' l =
  encode_' l 1

let test_encode' _ =
  let i = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  let o = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] in
  assert_equal ~msg:"encodes runs"
    (encode' i) o

let rec repeat x n =
  match n with
  | 1 -> [x]
  | _ -> x::(repeat x (n-1))

let rec decode l =
  match l with
  | [] -> []
  | (Many (n,x))::t -> (repeat x n) @ (decode t)
  | (One x)::t -> x::(decode t)

let test_decode _ =
  let i = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] in
  let o = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  assert_equal ~msg:"decodes runs"
    (decode i) o

let () =
  run_test_tt_main
    ("99" >:::
      ["last" >:: test_last
      ;"last_two" >:: test_last_two
      ;"at" >:: test_at
      ;"flatten" >:: test_flatten
      ;"compress" >:: test_compress
      ;"pack" >:: test_pack
      ;"encode" >:: test_encode
      ;"encode'" >:: test_encode'
      ;"decode" >:: test_decode
    ])
