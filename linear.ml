type size = int * int

type 'a matrix = 'a array array * size

let make rows cols =
  let matrix = Array.init rows (fun _ -> Array.make cols 0.) in
  (matrix, (rows, cols))

let rows (_, (rows, _)) =
  rows

let cols (_, (_, cols)) =
  cols

let row (m, _) i =
  m.(i-1)

let col (m, (rows, _)) j =
  Array.init rows (fun i -> m.(i).(j-1))

let get (m, _) i j =
  m.(i-1).(j-1)

let set (m, _) i j e =
  m.(i-1).(j-1) <- e

let iter_row m f i =
  for j = 1 to cols m do
    get m i j
  done
let iter_col m f j = for i = 1 to rows m do
    get m i j
  done

let iter_rows m f =
  for i = 1 to rows m do
    for j = 1 to cols m do
      f i j
    done
  done

let iter_cols m f =
  for j = 1 to cols m do
    for i = 1 to rows m do
      f i j
    done
  done

let scale (m, s) a =
  (Array.map (Array.map (( *.) a)) m, s)

let transpose m =
  let t = make (cols m) (rows m) in
  iter_rows m
    (fun i j -> set t j i (get m i j));
   t

let dot_product a b =
  let products = Array.map2 (fun x y -> x *. y) a b in
  Array.fold_left (+.) 0. products

let from_vector a =
  (Array.make 1 a, (1, Array.length a))

let pairwise a b f =
  let c = make (rows a) (rows b) in
  iter_rows a
    (fun i j ->
      let x = get a i j in
      let y = get b i j in
      set c i j (f x y));
  c

let add a b =
  pairwise a b ( +.)

let hadamard_product a b =
  pairwise a b ( *.)

let pairwise_row_col a b f =
  let c = make (rows a) (cols b) in
  iter_rows c
    (fun i j ->
      set c i j (f (row a i) (col b j)));
  c

let multiply a b =
  pairwise_row_col a b dot_product

let identity rows cols =
  let m = make rows cols in
  iter_rows m
    (fun i j ->
      if i = j then set m i j 1.);
  m

let inverse m = ()


let norm1 m =
  let v = row m 1 in
  Array.fold_left (fun a x -> a +. abs_float x) 0. v

let norm2 m =
  let v = row m 1 in
  let sqrd = Array.map (fun x -> x ** 2.) v in
  sqrt (Array.fold_left (+.) 0. sqrd)

let max_norm m =
  let v = row m 1 in
  Array.fold_left max 0. v

let frobenious_norm m =
  let a = ref 0. in
  iter_rows m
    (fun i j ->
       a := !a +. get m i j ** 2.);
  sqrt !a

let trace m =
  let a = ref 0. in
  iter_rows m
    (fun i j ->
      if i = j then a := !a +. get m i j);
  !a

let frobenious_norm' m =
  trace (multiply m (transpose m))

let iter_row_sep m f g =
  iter_rows m
    (fun i j ->
      f (get m i j);
      if j = cols m then g ())

let iter_col_sep m f g =
  iter_col m
    (fun i j ->
      f (get m i j);
      if i = rows m then g ())

let print m =
  iter_row_sep m
    (fun e -> print_float e; print_string "; ")
    (fun _ -> print_newline ())

let () =
  let m = make 3 2 in
  set m 2 1 1.;
  set m 1 2 1.;
  print m;
  print_newline ();
  print (transpose m);
  print_newline ();
  print (from_vector (row m 1));
  print_newline ();
  print (from_vector (col m 1));
  print_newline ();
  print_float (dot_product (row m 1) (row m 2));
  print_newline ();
  print_newline ();
  print (multiply m (transpose m));
  print_newline ();
  let n = add m m in
  print n;
  print_newline ();
  print (scale n 3.);
  print_newline ();
  print (identity 6 6)
