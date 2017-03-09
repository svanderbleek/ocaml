open Core.Std

let add_or_incr assoc key =
  let value = List.Assoc.find assoc key in
  match value with
  | None -> List.Assoc.add assoc key 1
  | Some count -> List.Assoc.add assoc key (count + 1)

let mkcounts () =
  In_channel.fold_lines stdin ~init:[] ~f:add_or_incr

let () =
  mkcounts ()
  |> List.sort ~cmp:(fun (_, x) (_, y) -> Int.descending x y)
  |> Fn.flip List.take 10
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
