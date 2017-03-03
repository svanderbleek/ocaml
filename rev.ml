try
  match Sys.argv with
  | [|_; fi; fo|] ->
    let chi = open_in fi in
    let cho = open_out fo in
    Revl.lines chi cho;
    close_in chi;
    close_out cho
  | _ ->
    print_string "rev <in> <out>";
    print_newline ()
with e ->
  print_string (Printexc.to_string e);
  print_newline ()
