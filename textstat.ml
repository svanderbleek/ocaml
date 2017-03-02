(* Text Stats *)

type stats = int * int * int * int

let lines (l, _, _, _) = l
let characters (_, c, _, _) = c
let words (_, _, w, _) = w
let sentences (_, _, _, s) = s

let stats_from_ch ch =
  let ls = ref 0 in
  let cs = ref 0 in
  let ws = ref 0 in
  let ss = ref 0 in
  try
    while true do
      let l = input_line ch in
      incr ls;
      cs := !cs + String.length l;
      String.iter
        (fun c ->
           match c with
           | '.' | '?' | '!' -> incr ss
           | ' ' -> incr ws
           | _ -> ())
        l
    done;
    (0, 0, 0, 0)
  with End_of_file -> (!ls, !cs, !ws, !ss)

let stats_from_f f =
  let ch = open_in f in
  let st = stats_from_ch ch in
  close_in ch;
  st
