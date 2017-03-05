let rec rev_ l acc =
  match l with
  | [] -> acc
  | h :: t ->
    rev_ t (h :: acc)

let rev' l =
  rev_ l []

let append' x y =
  rev_ (rev' x) y

let rec concat_ ll acc =
  match ll with
  | [] -> acc
  | h :: t ->
    concat_ t (append' acc h)

let concat' ll =
  concat_ ll []

let rec all_any_ ll acc =
  if acc then
    match ll with
    | [] -> acc
    | h :: t -> all_any_ t (List.mem true h)
  else acc

let all_any ll =
  all_any_ ll true

let bangs s =
  let b = ref 0 in
  let f c = if c = '!' then b := !b + 1 in
  String.iter f s;
  !b

let r_bang =
  String.map (fun c -> if c = '!' then '.' else c)

let s_cat =
  String.concat ""

let rec b_cat_ ls b =
  match ls with
  | [] -> Buffer.contents b
  | h :: t ->
    Buffer.add_string b h;
    b_cat_ t b

let b_cat ls =
  b_cat_ ls (Buffer.create 16)

let rec cntns_ s si x xi n =
  try
    let _ = read_line () in
    if si < String.length s then
      if xi < String.length x then
        let si' = String.index_from s si (String.get x xi) in
        if xi = 0 then cntns_ s si' x 1 n else
        if si' = si + 1
        then cntns_ s si' x (xi+1) n
        else cntns_ s (si'-xi) x 0 n
      else cntns_ s si x 0 (n+1)
    else n
  with Not_found -> n

let cntns s x =
  cntns_ s 0 x 0 0
