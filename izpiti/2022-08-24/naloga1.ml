(* 1. a) *)
let zamenjaj ((a, b), (c, d)) = ((a, c), (b, d))

(* 1. b) *)
let modus trojica =
  let x, y, z = trojica in
  if x = y || x = z then Some x else if y = z then Some y else None

(* 1. c) *)
let uncons = function [] -> None | gl :: rep -> Some (gl, rep)

(* 1. d) *)
let rec vstavljaj element = function
  | [] -> []
  | [ gl ] -> [ gl ]
  | gl :: rep -> gl :: element :: vstavljaj element rep

(* 1. e) *)
let popolnoma_obrni gnezden_seznam =
  let rec obrni acc = function
    | [] -> acc
    | gl :: rep -> obrni (gl :: acc) rep
  in
  let rec uporabi_in_obrni f acc = function
    | [] -> acc
    | gl :: rep -> uporabi_in_obrni f (f gl :: acc) rep
  in
  uporabi_in_obrni (obrni []) [] gnezden_seznam
