(*PRVA NALOGA*)

let pitagorejska_trojica (a, b, c) = c * c = (b * b) + (a * a)

let priblizek_korena x =
  let floaty = float_of_int x in
  let koren = sqrt floaty in
  let priblizek = floor koren in
  int_of_float priblizek

let obrni list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> pomozna (gl :: acc) rep
  in
  pomozna [] list

let izpisi_soda_liha seznam =
  let rec pomozna soda liha = function
    | [] -> obrni soda @ obrni liha
    | gl :: rep ->
        if gl mod 2 = 0 then pomozna (gl :: soda) liha rep
        else pomozna soda (gl :: liha) rep
  in
  let po_vrsti = pomozna [] [] seznam in
  let rec izpis = function
    | [] -> ()
    | gl :: rep ->
        print_int gl;
        izpis rep
  in
  izpis po_vrsti

let alt_konst seznam = 
  let rec pomozna prejsnja = function 
  | [] -> true
  | gl::rep -> (match gl with 
    |None -> (match prejsnja with Some y -> pomozna  gl rep | None -> false)
    |Some x -> (match prejsnja with None -> pomozna gl rep | Some y -> false)
  )
in  match seznam with 
  | [] -> true
  | gl:: rep -> pomozna gl rep

let mini x fs =
  let rec pomozna indeks favorit = function 
  | [] -> favorit
  | gl::rep -> (
    match favorit with 
    | None -> pomozna (indeks + 1) (Some (gl x, indeks)) rep 
    | Some (vrednost, j) -> if gl x >= vrednost then pomozna (indeks + 1) (Some (vrednost, j)) rep else pomozna (indeks + 1) (Some (gl x, indeks)) rep
  )
in 
match pomozna 0 None fs with 
| None -> None 
| Some (v, i) -> Some i

(*DRUGA NALOGA*)

