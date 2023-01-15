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
    | gl :: rep -> (
        match gl with
        | None -> (
            match prejsnja with Some y -> pomozna gl rep | None -> false )
        | Some x -> (
            match prejsnja with None -> pomozna gl rep | Some y -> false ) )
  in
  match seznam with [] -> true | gl :: rep -> pomozna gl rep

let mini x fs =
  let rec pomozna indeks favorit = function
    | [] -> favorit
    | gl :: rep -> (
        match favorit with
        | None -> pomozna (indeks + 1) (Some (gl x, indeks)) rep
        | Some (vrednost, j) ->
            if gl x >= vrednost then
              pomozna (indeks + 1) (Some (vrednost, j)) rep
            else pomozna (indeks + 1) (Some (gl x, indeks)) rep )
  in
  match pomozna 0 None fs with None -> None | Some (v, i) -> Some i

(*DRUGA NALOGA*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree

let primer =
  Node
    ( Node (Empty, (3, Node (Empty, "g", Node (Empty, "t", Empty))), Empty),
      (7, Node (Empty, "a", Empty)),
      Node
        ( Empty,
          ( 10,
            Node
              ( Node (Empty, "e", Empty),
                "r",
                Node (Empty, "z", Node (Empty, "t", Empty)) ) ),
          Empty ) )

let rec obicajen_iskalnik element = function
  | Empty -> false
  | Node (l, v, d) ->
      if v = element then true
      else if element < v then obicajen_iskalnik element l
      else obicajen_iskalnik element d

let rec iskalnik (a, b) = function
  | Empty -> false
  | Node (left, (vozlisce, subtree), right) ->
      if vozlisce = a then obicajen_iskalnik b subtree
      else if a < vozlisce then iskalnik (a, b) left
      else iskalnik (a, b) right

let rec iskalnik_prve a = function
  | Empty -> false
  | Node (l, (v, sub), r) ->
      if v = a then true
      else if a < v then iskalnik_prve a l
      else iskalnik_prve a r

let rec subtree_finder element = function
  | Empty -> None
  | Node (l, (v, sub), r) -> (
      if v = element then Some sub
      else if element < v then
        match subtree_finder element l with Some x -> Some x | None -> None
      else match subtree_finder element r with Some y -> Some y | None -> None )

(*let vstavljanje (a, b) drevo =
  let rec popravljam_subtree = function
    | Empty -> Node (Empty, b, Empty)
    | Node (l, v, r) ->
        if l = Empty then
          let levi = popravljam_subtree l in
          Node (levi, v, r)
        else
          let desni = popravljam_subtree r in
          Node (l, v, desni)
  in
  let rec popravljam = function
    | Empty -> Node (Empty, (a, popravljam_subtree Empty), Empty)
    | Node (l, (v, sub), d) ->
        if v = a then Node (l, (v, popravljam_subtree sub), d)
        (*preverimo, če je a v levem, sicer se preselimo na desnega*)
        else if
          iskalnik_prve a l
        then (
          let levi = popravljam l in
          Node (levi, (v, sub), d)
        )
        else
          let desni = popravljam d in
          Node (l, (v, sub), desni)
  in
  (*preverimo, če že obstaja*)
  if iskalnik (a, b) drevo then drevo 
  (*sicer popravimo drevo*)
  else popravljam drevo
*)

let rec vstavljanje (a, b) drevo =
  let rec dodaj element = function
    | Empty -> Node (Empty, element, Empty)
    | Node (l, v, d) ->
        if v = element then Node (l, v, d)
        else if element < v then dodaj element l
        else dodaj element d
  in
  if iskalnik (a, b) drevo then drevo
  else
    match drevo with
    | Empty -> Node (Empty, (a, Node (Empty, b, Empty)), Empty)
    | Node (l, (v, sub), d) ->
        if v = a then Node (l, (v, dodaj b sub), d)
        else if a < v then vstavljanje (a, b) l
        else vstavljanje (a, b) d

let lexi_fold f zacetna drevo =
  let rec majhno_racunanje acc kljuc vejica =
    (*rezultati pod istim ključem*)
    match vejica with
    | Empty -> acc
    | Node (levo_drevo, koren, desno_drevo) ->
        (*najprej izračunamo levo vrednost - ohranimo akumulacijo rezultata,
           potem vozlisce - acc bo leva vrednost, potem desno z vozliscno vrednostjo v acc
           - dosežemo leksikografsko ureditev*)
        let leva_vrednost = majhno_racunanje acc kljuc levo_drevo in
        let vozliscna_vrednost = f leva_vrednost kljuc koren in
        majhno_racunanje vozliscna_vrednost kljuc desno_drevo
  in

  let rec racunanje_po_kljucih acc = function
    | Empty -> acc
    | Node (l, (kljuc, subtree), d) ->
        let levi_izracun = racunanje_po_kljucih acc l in
        let vozliscni_izracun = majhno_racunanje levi_izracun kljuc subtree in
        racunanje_po_kljucih vozliscni_izracun d
  in
  racunanje_po_kljucih zacetna drevo

let generator_seznama lexi_tree =
  let rec generator_parov acc kljuc =
    (*vrne obratno lex ureditev parov pod izbranim kljucem*) function
    | Empty -> acc
    | Node (leva_veja, koren, desna_veja) ->
        let levi_pari = generator_parov acc kljuc leva_veja in
        let korenski_pari = (kljuc, koren) :: levi_pari in
        generator_parov korenski_pari kljuc desna_veja
  in
  let rec sprehod_po_kljucih acc = function
    | Empty -> obrni acc
    | Node (leva_veja, (kljuc, subtree), desna_veja) ->
        let levi_seznam = sprehod_po_kljucih acc leva_veja in
        let seznam_v_kljucu = generator_parov levi_seznam kljuc subtree in
        sprehod_po_kljucih seznam_v_kljucu desna_veja
  in
  sprehod_po_kljucih [] lexi_tree

(*TRETJA NALOGA*)
