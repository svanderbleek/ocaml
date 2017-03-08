open Core.Std

type table =
  {head: string list
  ;rows: string list list}

let max_widths t =
  let lengths l = List.map l ~f:String.length in
  List.fold
    (List.map t.rows ~f:lengths)
    ~init:(lengths t.head)
    ~f:(List.map2_exn ~f:Int.max)

let pad_l (l, s) =
  let padding = String.make (l - String.length s) ' ' in
  " " ^ s ^ padding ^ " "

let pad w r =
  List.map (List.zip_exn w r) ~f:pad_l

let render_row r =
  "|" ^ String.concat ~sep:"+" r ^ "|\n"

let row_sep w =
  let padded = List.map w ~f:((+) 2) in
  render_row (List.map padded ~f:(Fn.flip String.make '-'))

let render_table t =
  let widths = max_widths t in
  let padder = pad widths in
  let head = padder t.head in
  let rows = List.map t.rows ~f:padder in
  String.concat
    ~sep:(row_sep widths)
    (List.map (head :: rows) ~f:render_row)

let () =
  let table =
  {head =
    ["language"; "architect"; "first release"]
  ;rows = [
    ["Lisp"; "John McCarthy"; "1958"];
    ["C"; "Dennis Ritchie"; "1969"];
    ["ML"; "Robin Milner"; "1973"];
    ["OCaml"; "Xavier Leroy";"1996"];
  ]} in
  printf "%s\n" (render_table table)
