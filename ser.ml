try
  match Sys.argv with
  | [|_; f|] ->
    let ch = open_in f in
    Serl.lines ch;
    close_in ch
  | _ ->
    print_string "ser <in> <match>";
    print_newline ()
with e ->
  print_string (Printexc.to_string e);
  print_newline ()
