type input =
  {pos_in: unit -> int
  ;seek_in: int -> unit
  ;input_char: unit -> char
  ;input_char_opt: unit -> char option
  ;in_channel_length: int}

let input_char_a a i () =
  if !i > Array.length a - 1
  then raise End_of_file
  else begin
    let c = a.(!i) in
    incr i;
    c
  end

let input_of_array a =
  let i = ref 0 in {pos_in = (fun () -> !i)
  ;seek_in =
    (fun p ->
      if p < 0
      then raise (Invalid_argument "p")
      else i := p)
  ;input_char = input_char_a a i
  ;input_char_opt =
    (fun () ->
      try Some (input_char_a a i ()) with End_of_file -> None)
  ;in_channel_length = Array.length a}

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch)
  ;seek_in = seek_in ch
  ;input_char = (fun () -> input_char ch)
  ;input_char_opt =
    (fun () ->
      try Some (input_char ch) with End_of_file -> None)
  ;in_channel_length = in_channel_length ch}

let seek_in_s pos p =
  if p < 0
  then raise (Invalid_argument "p")
  else pos := p

let in_char_s pos s () =
  if !pos > String.length s - 1
  then raise End_of_file
  else
    let c = s.[!pos] in
    incr pos;
    c

let input_of_string s =
  let pos = ref 0 in
  {pos_in = (fun () -> !pos)
  ;seek_in = seek_in_s pos
  ;input_char = in_char_s pos s
  ;input_char_opt =
    (fun () ->
      try Some (in_char_s pos s ()) with End_of_file -> None)
  ;in_channel_length = String.length s}

let rewind i =
  i.seek_in (i.pos_in () - 1)

let is_non_letter x =
  match x with
  | ' ' | '!' | '(' | ')'
  | '.' | ',' | ';' | ':' -> true
  |_ -> false

let rec skip_characters i =
  if is_non_letter (i.input_char ())
  then skip_characters i
  else rewind i

let rec collect_characters b i =
  match
    try Some (i.input_char ()) with
    | End_of_file -> None
  with
  | None -> Buffer.contents b
  | Some c ->
    if is_non_letter c
    then Buffer.contents b
    else begin
      Buffer.add_char b c;
      collect_characters b i
    end

let read_word i =
  try
    skip_characters i;
    Some (collect_characters (Buffer.create 20) i)
  with
  | End_of_file -> None

let rec read_words_ i a =
  match read_word i with
  | None -> List.rev (List.map String.lowercase_ascii a)
  | Some w -> read_words_ i (w :: a)

let read_words i =
  read_words_ i []

type output =
  {output_char: char -> unit
  ;output_channel_length: unit -> int}

let output_of_buffer b =
  {output_char = (fun c -> Buffer.add_char b c)
  ;output_channel_length = (fun () -> Buffer.length b)}

let output_of_channel ch =
  {output_char = output_char ch
  ;output_channel_length = (fun () -> out_channel_length ch)}

let output_char_s pos s c =
  if !pos < String.length s
  then begin
    s.[!pos] <- c;
    incr pos
  end else raise End_of_file

let output_of_string s =
  let pos = ref 0 in
  {output_char = output_char_s pos s
  ;output_channel_length = (fun () -> String.length s)}

let output_int_list o l =
  o.output_char '[';
  List.iteri
    (fun i n ->
      String.iter o.output_char (string_of_int n);
      if i < List.length l - 1 then begin
        o.output_char ';';
        o.output_char ' '
      end)
    l;
  o.output_char ']'

type line_input =
  {pos_in: unit -> int
  ;seek_in: int -> unit
  ;input_char: unit -> char
  ;in_channel_length: int}

let input_string (i:line_input) n =
  let b = Buffer.create 100 in
  try
    for _ = 1 to n do
      Buffer.add_char b (i.input_char ())
    done;
    Buffer.contents b
  with
  | End_of_file -> Buffer.contents b

let line_input_of_ch ch =
  {pos_in = (fun () -> pos_in ch)
  ;seek_in = seek_in ch
  ;input_char =
    (fun () ->
      match input_char ch with
      | '\n' -> raise End_of_file
      | c -> c)
  ;in_channel_length = in_channel_length ch}
