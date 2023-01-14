(* 1. a) *)
let je_idempotent matrika =
  let (a, b), (c, d) = matrika in
  let k1 = (a * a) + (b * c)
  and k3 = 2 * a * c
  and k2 = b * (a * d)
  and k4 = (c * b) + (a * d) in
  k1 = a && k2 = b && k3 = c && k4 = d

(* 1. b) *)
let produkt list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> if gl = 0 then pomozna acc rep else pomozna (gl * acc) rep
  in
  pomozna 1 list

(* 1. c) *)

let obrni list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> pomozna (gl :: acc) rep
  in
  pomozna [] list

let stalin_sort list =
  let rec primerjaj acc element = function
    | [] -> acc
    | gl :: rep -> primerjaj (element > gl) element rep
  in
  let rec pomozna acc = function
    | [] -> obrni acc
    | gl :: rep ->
        if primerjaj true gl acc = true then pomozna (gl :: acc) rep
        else pomozna acc rep
  in
  pomozna [] list

(* 1. d) *)
let splosni_collatz f g p z k =
  let rec pomozna acc trenutni =
    if trenutni = k then obrni acc
    else if p trenutni then pomozna (f trenutni :: acc) (f trenutni)
    else pomozna (g trenutni :: acc) (g trenutni)
  in
  pomozna [] z
