let len_v (x, y) =
  sqrt (x ** 2. +. y ** 2.)

let add_v (ax, ay) (bx, by) =
  (ax +. bx, ay +. by)

let sub_v (ax, ay) (bx, by) =
  (ax -. bx, ay -. by)

let mul_v m (x, y) =
  (m *. x, m *. y)

let scale_v s ((x, y) as v) =
  let l = len_v v in
  if l = 0. then v
  else mul_v (s /. l) v

let eq_d v1 v2 =
  let v = sub_v v2 v1 in
  let hv = mul_v (1. /. 2.) v in
  add_v v1 hv

let sep f =
  let i = floor f in
  (i, f -. i)

let star f =
  let col = floor (49. *. f) in
  for i = 1 to int_of_float col do
    print_string " "
  done;
  print_string "*";
  print_newline ()

let star' f =
  let col = floor (49. *. f) in
  for i = (-48) to int_of_float col do
    print_string " "
  done;
  print_string "*";
  print_newline ()

let plot f x y s =
  let steps = int_of_float ((y -. x) /. s) in
  for i = 0 to steps do
    star (f (x +. (float_of_int i *. s)))
  done

let plot' f x y s =
  let steps = int_of_float ((y -. x) /. s) in
  for i = 0 to steps do
    star' (f (x +. (float_of_int i *. s)))
  done
