type input =
  {pos_in: unit -> int
  ;seek_in: int -> unit
  ;input_char: unit -> char
  ;in_channel_length: int}

type input_bits =
  {input: input
  ;mutable byte: int
  ;mutable bit: int}

let input_of_channel c =
  {pos_in = (fun () -> pos_in c)
  ;seek_in = seek_in c
  ;input_char = (fun () -> input_char c)
  ;in_channel_length = in_channel_length c}

let input_bits_of_input i =
  {input = i
  ;byte = 0
  ;bit = 0}

let rec getbit b =
  if b.bit = 0 then begin
  b.byte <- int_of_char (b.input.input_char ());
  b.bit <- 128;
  getbit b
  end else
  let n = b.byte land b.bit in
  b.bit <- b.bit / 2;
  if n > 0 then 1 else 0

let align b =
  b.bit <- 0

let getval b s =
  let a = ref 0 in
  for o = s - 1 downto 0 do
    let n = getbit b in
    a := !a lor (n lsl o)
  done;
  !a

let geval' b s =
  if s = 8 && b.bit = 0
  then int_of_char (b.input.input_char ())
  else getval b s

let getval_32 b =
  let i = ref Int32.zero in
  for o = 31 downto 0 do
    let n = getbit b in
    if n = 1 then
    i := Int32.logor !i (Int32.shift_left Int32.one o)
  done;
  !i

let print_header f =
  let ch = open_in_bin f in
  let i = input_of_channel ch in
  let b = input_bits_of_input i in
  let src_port = getval b 16 in
  let dest_port = getval b 16 in
  let seq_number = getval_32 b in
  let ack_number = getval_32 b in
  let _ = getval b 4 in
  let _ = getval b 6 in
  let urgent = getbit b in
  let ack = getbit b in
  let push = getbit b in
  let reset = getbit b in
  let syn = getbit b in
  let fin = getbit b in
  let receive = getval b 16 in
  let checksum = getval b 16 in
  let urgent_pointer = getval b 16 in
  print_string "Source port = ";
  print_int src_port;
  print_string "\nDestination = ";
  print_int dest_port;
  print_string "\nSequence =";
  print_string (Int32.to_string seq_number);
  print_string "\nAcknowledgement Number = ";
  print_string (Int32.to_string ack_number);
  print_string "\nFlags:\n Urgent = "; print_int urgent;
  print_string "\nAck = "; print_int ack;
  print_string "\nPush = "; print_int push;
  print_string "\nReset = "; print_int reset;
  print_string "\nSyn = "; print_int syn;
  print_string "\nFin = "; print_int fin;
  print_string "Receive window size = ";
  print_int receive;
  print_string "\nChecksum = ";
  print_int checksum;
  print_string "\nUrgent pointer = ";
  print_int urgent_pointer;
  print_string "\n";
  close_in ch

type output =
  {output_char: char -> unit
  ;out_channel_length: unit -> int}

let output_of_channel c =
  {output_char = output_char c
  ;out_channel_length = (fun () -> out_channel_length c)}

type output_bits =
  {output: output
  ;mutable byte: int
  ;mutable bit: int}

let output_bits_of_output o =
  {output = o
  ;byte = 0
  ;bit = 7}

let flush o =
  if o.bit < 7
  then o.output.output_char (char_of_int o.byte);
  o.byte <- 0;
  o.bit <- 7

let rec putbit o b =
  if o.bit = (-1) then begin
  flush o;
  putbit o b
  end else begin
  if b <> 0 then o.byte <- o.byte lor (1 lsl o.bit);
  o.bit <- o.bit - 1
  end

let putval o v l =
  for i = l - 1 downto 0 do
    putbit o (v land (1 lsl i))
  done

let putval' o v l =
  if l = 8 && o.bit = 7
  then o.output.output_char (char_of_int v)
  else putval o v l

let putval_32 b i s =
  for o = s - 1 downto 0 do
    putbit b
      (Int32.to_int (Int32.logand i (Int32.shift_left (Int32.of_int o) 1)))
  done

let output_header f =
  let ch = open_out_bin f in
  let o = output_of_channel ch in
  let bits = output_bits_of_output o in
  putval bits 38 16;
  putval bits 47892 16;
  putval_32 bits 1656212531l 32;
  putval_32 bits 1481973485l 32;
  putval bits 5 4;
  putval bits 0 6;
  putbit bits 0;
  putbit bits 0;
  putbit bits 0;
  putbit bits 0;
  putbit bits 0;
  putbit bits 0;
  putval bits 17664 16;
  putval bits 888 16;
  putval bits 63404 16;
  flush bits;
  close_out ch
