let small2 x =
  let y = ref 1 in
  while !y < x do
    y := !y * 2
  done;
  !y

let p_hist a =
  print_string "hist:";
  print_newline ();
  for i = 0 to (Array.length a) - 1 do
    if a.(i) > 0
    then begin
      print_char (char_of_int i);
      print_string ": ";
      print_int a.(i);
      print_newline ()
    end
  done

let s_stat w s a c =
    let i = int_of_char c in
    a.(i) <- a.(i) + 1;
    match c with
      '.' | '?' | '!' ->
      s := !s + 1
    | ' ' ->
      w := !w + 1
    | _ -> ()

let i_stat i =
  let l = ref 0 in
  let c = ref 0 in
  let w = ref 0 in
  let s = ref 0 in
  let h = Array.make 256 0 in
  try
    while true do
      let n = input_line i in
      l := !l + 1;
      c := !c + String.length n;
      String.iter (s_stat w s h) n
    done
  with
  | End_of_file ->
    print_string "lines: ";
    print_int !l;
    print_newline ();
    print_string "chars: ";
    print_int !c;
    print_newline ();
    print_string "words: ";
    print_int !w;
    print_newline ();
    print_string "sentences: ";
    print_int !s;
    print_newline ();
    p_hist h

let f_stat f =
  let i = open_in f in
  try i_stat i with
  | _ -> ();
  close_in i


let rec f_a a i t f =
  if i < t
  then begin
    f a.(i);
    f_a a (i+1) t f
  end else ()

let sum_a a =
  let s = ref 0 in
  f_a a 0 (Array.length a)
    (fun i -> s := !s + i);
  !s

let r_a a =
  if a <> [||] then
  let l = (Array.length a) - 1 in
  for i = 0 to l / 2 do
    let l' = l - i in
    let i' = a.(i) in
    a.(i) <- a.(l');
    a.(l') <- i'
  done

let tbl n =
  let a = Array.make n [||] in
  for i = 1 to n do
    a.(i - 1) <- Array.make n 0;
    for j = 1 to n do
      a.(i - 1).(j - 1) <- i * j
    done;
  done;
  a

let upc c =
  let i = int_of_char c in
  if 97 <= i && i <= 122
  then char_of_int (i - 32)
  else c

let loc c =
  let i = int_of_char c in
  if 65 <= i && i <= 90
  then char_of_int (i + 32)
  else c
