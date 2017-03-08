open Core.Std

type table =
  {head: string list
  ;rows: string list list}

let max_widths t =
  let lengths = List.map ~f:String.length in
  let widths = lengths t.head in
  let rows = List.map t.rows ~f:lengths in
  List.fold rows ~init:widths ~f:(List.map2_exn ~f:Int.max)

let pad p (w, r) =
  " " ^ r ^ String.make (w - String.length r) p ^ " "

let renderer widths s p row =
  let padded = List.map (List.zip_exn widths row) ~f:(pad p) in
  "|" ^ String.concat ~sep:s padded ^ "|\n"

let render_table t =
  let widths = max_widths t in
  let render_row = renderer widths in
  render_row "|" ' ' t.head ^
  render_row "+" '-' (List.map t.head ~f:(Fn.const "")) ^
  String.concat (List.map t.rows ~f:(render_row "|" ' '))

let mktable () =
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

let to_ext file =
  let maybe_fname = String.rsplit2 file ~on:'/' in
  match maybe_fname with
  | None | Some ("", _) -> None
  | Some (_, fname) ->
    let maybe_ext = String.rsplit2 fname ~on:'.' in
    match maybe_ext with
    | None | Some ("", _) -> None
    | Some (_, ext) -> Some ext

let rec render_exts dir =
  let files = List.map (Sys.ls_dir dir) ~f:((^/) dir) in
  let exts = List.filter_map files ~f:to_ext in
  let dirs = List.filter files ~f:(Fn.non Sys.is_file_exn) in
  List.fold dirs ~init:exts ~f:(fun acc dir -> acc @ render_exts dir)

let mkexts () =
  print_string (String.concat ~sep:"," (render_exts "."));
  print_newline ()

let () =
  match Sys.argv with
  | [|_; "table"|] ->
    mktable ()
  | [|_; "exts"|] ->
    mkexts ()
  | _ ->
    print_string "valid commands: table, exts";
    print_newline ()
