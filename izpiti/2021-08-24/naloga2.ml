(*============================================================================*]
  Pri tej nalogi bomo za slovar uporabili kar enostavno implementacijo z 
  asociativnim seznamom, ki smo jo spoznali na predavanjih.
  S spodaj definiranimi funkcijami si lahko pomagate pri vseh podnalogah.
[*============================================================================*)

type ('a, 'b) slovar = ('a * 'b) list

let prazen_slovar : ('a, 'b) slovar = []

let velikost (m : ('a, 'b) slovar) = List.length m

let vsebuje (x : 'a) (m : ('a, 'b) slovar) = List.mem_assoc x m

(* Vrne vrednost, ki pripada ključu ali None *)
let najdi x (m : ('a, 'b) slovar) = List.assoc_opt x m

(* Doda vrednost v slovar in povozi prejšnjo, če obstaja *)
let dodaj (k, v) (m : ('a, 'b) slovar) = (k, v) :: List.remove_assoc k m

(*============================================================================*]
  Matematične izraze predstavimo z dvojiškimi drevesi, v katerih vozlišča predstavljajo 
  aritmetične operacije, listi pa števila ali spremenljivke, predstavljene z nizi.
  Izraz v drevo pretvorimo tako, da pri operaciji levi podizraz vzamemo za levo 
  poddrevo, desni podizraz za desno, v vozlišče pa zapišemo operator.
[*============================================================================*)

type operator = Plus | Minus | Krat | Deljeno

type 'a izraz =
  | Spremenljivka of string
  | Konstanta of 'a
  | Operacija of ('a izraz * operator * 'a izraz)

(* (x - 3)- (y * (z / x))  *)
let primer =
  Operacija
    ( Operacija (Spremenljivka "x", Minus, Konstanta 3),
      Minus,
      Operacija
        ( Spremenljivka "y",
          Krat,
          Operacija (Spremenljivka "z", Deljeno, Spremenljivka "x") ) )

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `prestej : izraz -> int`, ki vrne število vseh "različnih" 
  spremenljivk v izrazu.
[*----------------------------------------------------------------------------*)

let prestej izraz =
  let rec pomozna acc spremenljivke = function
    | Konstanta _ -> (acc, spremenljivke)
    | Spremenljivka x ->
        if List.mem x spremenljivke then (acc, spremenljivke)
        else (acc + 1, x :: spremenljivke)
    | Operacija (levi, operator, desni) ->
        let levi_acc, leve_spr = pomozna 0 spremenljivke levi in
        let desni_acc, desne_spr = pomozna levi_acc leve_spr desni in
        (acc + desni_acc, spremenljivke @ desne_spr)
  in
  fst (pomozna 0 [] izraz)

(* b *)
(*----------------------------------------------------------------------------*]
Napišite funkcijo `izlusci : 'a izraz -> (string * int) slovar`, ki sprejme izraz 
in vrne slovar, ki pove, kolikokrat se posamezna spremenljivka pojavi v izrazu. 
Vrstni red v slovarju ni pomemben.
[*----------------------------------------------------------------------------*)

let izlusci izraz =
  let rec pomozna acc = function
    | Konstanta _ -> acc
    | Spremenljivka x -> (
        match najdi x acc with
        | None -> dodaj (x, 1) acc
        | Some vrednost -> dodaj (x, vrednost + 1) acc )
    | Operacija (l, o, d) ->
        let levi_slovar = pomozna acc l in
        let desni_slovar = pomozna levi_slovar d in
        desni_slovar
  in
  pomozna prazen_slovar izraz

(* c *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izracunaj : (string * int) slovar -> int izraz -> option int`, 
  ki sprejme izraz in slovar vrednosti spremenljivk ter poskuša izračunati vrednost 
  izraza. Če to ni mogoče (deljenje z 0 ali manjkajoča definicija spremenljivke), 
  naj bo rezultat `None`. 
    # izracunaj [("x",3); ("y", 4); ("z",5)] primer;;
    - : int option = Some (-4)
[*----------------------------------------------------------------------------*)

let izracun operacija x y =
  match operacija with
  | Plus -> Some (x + y)
  | Krat -> Some (x * y)
  | Minus -> Some (x - y)
  | Deljeno -> if y = 0 then None else Some (x / y)

let izracunaj slovar_vrednosti izraz =
  let rec pomozna acc = function
    | Konstanta a -> Some (a + acc)
    | Spremenljivka x -> (
        match najdi x slovar_vrednosti with
        | None -> None
        | Some y -> Some (acc + y) )
    | Operacija (l, o, d) -> (
        match pomozna 0 l with
        | None -> None
        | Some levi -> (
            match pomozna 0 d with
            | None -> None
            | Some desni -> izracun o levi desni ) )
  in
  pomozna 0 izraz

(* c *)
(*----------------------------------------------------------------------------*]
  Ocenite časovno zahtevnost funkcije `izracunaj` v odvisnosti od velikosti 
  izraza `n` (torej števila vseh vozlišč in listov v drevesu) ter števila različnih 
  spremenljivk `m`.
  Kako se časovna zahtevnost spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?
[*----------------------------------------------------------------------------*)
