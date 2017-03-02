let entry_to_ch ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

let dict_to_ch ch d =
  List.iter (entry_to_ch ch) d

let dict_to_f f d =
  let ch = open_out f in
  dict_to_ch ch d;
  close_out ch

let entry_of_ch ch =
  let k = input_line ch in
  let v = input_line ch in
  (int_of_string k, v)

let rec dict_of_ch ch =
  try
    let e = entry_of_ch ch in
    e :: dict_of_ch ch
  with
  | End_of_file -> []

let dict_of_f f =
  let ch = open_in f in
  let d = dict_of_ch ch in
  close_in ch;
  d

let rec r3 _ =
  print_string "entry:";
  print_newline ();
  try
    let a = read_int () in
    let b = read_int () in
    let c = read_int () in
    (a,b,c)
  with
  | Failure _ ->
    print_string "retry";
    print_newline ();
    r3 ()

let pcol x y =
  for i = 1 to x do
    print_int (i * y);
    print_string "\t"
  done;
  print_newline ()

let rec tt x y =
  if x = y then () else begin
    pcol x y;
    tt x (y+1)
  end

let times_table x =
  tt x 1

let rec count_lines ch c =
  try
    let _ = input_line ch in
    count_lines ch (c+1)
  with
  | End_of_file -> c

let lines f =
  try
    let ch = open_in f in
    let c = count_lines ch 0 in
    close_in ch;
    c
  with
  | Sys_error _ -> 0

let rec copy_lines i o =
  try
    let l = input_line i in
    output_string o (l^"\n");
    copy_lines i o
  with
  | End_of_file -> ()


let copy i o =
  try
    let chi = open_in i in
    let cho = open_out o in
    copy_lines chi cho;
    close_in chi;
    close_out cho
  with
  | Sys_error _ -> ()

