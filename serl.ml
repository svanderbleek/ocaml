(*
  Knuth-Morris-Pratt
  s[b+i] != m[i]
  b = b + i - t[i]
  todo get it right
*)

let table m =
  let l = String.length m in
  let t = Array.make l 0 in
  t.(0) <- (-1);
  let i = ref 2 in
  let j = ref 0 in
  while !i < l do
    t.(!i) <- !j;
    if String.get m !i = String.get m !j
    then incr j
    else j := 0;
    incr i
  done;
  t

let matches l li m mi =
  false

let lines ch m =
  try
    while true do
      let l = input_line ch in
      if matches l 0 m 0 then begin
        print_string l;
        print_newline ()
      end
    done
  with End_of_file -> ()
