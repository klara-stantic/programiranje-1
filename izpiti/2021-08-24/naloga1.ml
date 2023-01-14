(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `je_urejena : int * int * int -> bool`, ki pove, ali je 
  podana trojica celih števil urejena strogo naraščajoče.
[*----------------------------------------------------------------------------*)

let je_urejena trojica =
  let a, b, c = trojica in
  if a >= b then false else if b >= c then false else true

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `poskusi_deljenje : float option -> float option -> float option`, 
  ki sprejme morebitni deljenec in morebitni delitelj ter vrne rezultat deljenja, 
  če se to da, ali pa `None`, če ne (kadar kakšnega argumenta ni ali pa bi prišlo 
  do deljenja z nič). 
  
    # poskusi_deljenje (Some 1.0) (Some 2.0);;
    - : float option = Some 0.5
    # poskusi_deljenje (Some 1.0) (Some 0.0);;
    - : float option = None
    # poskusi_deljenje None (Some 2.0);;
    - : float option = None
[*----------------------------------------------------------------------------*)

let poskusi_deljenje x y =
  match y with
  | None | Some 0. -> None
  | Some imenovalec -> (
      match x with None -> None | Some stevec -> Some (stevec /. imenovalec) )

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo `zavrti : 'a list -> int -> 'a list`, ki seznam zavrti 
  za dano število mest v levo (v vsaki rotaciji se prvi element prestavi na 
  konec seznama).
  
    # zavrti [1; 2; 3; 4; 5] 2;;
    - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec zavrti seznam k =
  match k with
  | k when k <= 0 -> seznam
  | k -> (
      match seznam with [] -> [] | gl :: rep -> zavrti (rep @ [ gl ]) (k - 1) )

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`, 
  ki sprejme cenilno funkcijo in seznam elementov. Vrne naj trojico, kjer so na prvem 
  mestu vsi elementi za katere je cenilna funkcija negativna, na drugem vsi, kjer 
  je enaka 0, na tretjem pa vsi preostali elementi.
  Elementi naj v seznamih nastopajo v enakem vrstnem redu kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.
  
    # razdeli ((-) 3) [1; 2; 3; 4; 5; 6];;
    - : int list * int list * int list = ([4; 5; 6], [3], [1; 2])
[*----------------------------------------------------------------------------*)

let obrni list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> pomozna (gl :: acc) rep
  in
  pomozna [] list

let razdeli f list =
  let rec pomozna acc1 acc2 acc3 = function
    | [] -> (obrni acc1, obrni acc2, obrni acc3)
    | x :: rep ->
        if f x < 0 then pomozna (x :: acc1) acc2 acc3 rep
        else if f x = 0 then pomozna acc2 (x :: acc2) acc3 rep
        else pomozna acc1 acc2 (x :: acc3) rep
  in
  pomozna [] [] [] list
