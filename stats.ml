(* CL Text Stats *)

try
  match Sys.argv with
  | [|_; f|] ->
    let s = Textstat.stats_from_f f in
    print_string "W: ";
    print_int (Textstat.words s);
    print_newline ();
    print_string "C: ";
    print_int (Textstat.words s);
    print_newline ();
    print_string "S: ";
    print_int (Textstat.words s);
    print_newline ();
    print_string "L: ";
    print_int (Textstat.words s);
    print_newline ();
  | _ ->
    print_string "U: stats <f>";
    print_newline ()
with e ->
  print_string "E: ";
  print_string (Printexc.to_string e);
  print_newline ();
  exit 1
