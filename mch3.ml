type point =
  {x: float; y: float}

let mirror {x; y} =
  {y; x}

let ptime () =
  let
  {Unix.tm_min
  ;Unix.tm_hour}
  =
  Unix.localtime (Unix.time ())
  in
  "It is "
  ^ string_of_int tm_hour
  ^ ":"
  ^ string_of_int tm_min

type r =
  {a: int; b: int; c: string; d: string; e: float; f: float}
