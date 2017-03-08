open Core.Std

let rec read_and_acc a =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> a
  | Some x -> read_and_acc (a +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_acc 0.)
