let lines i o =
  try
    while true do
      let l = input_line i in
      let r = ref "" in
      String.iter (fun c -> r := (String.make 1 c) ^ !r) l;
      output_string o (!r ^ "\n")
    done
  with End_of_file -> ()
