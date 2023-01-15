type 'a tape = Tape of { left : 'a list; head : 'a; right : 'a list }

type 'a command = Left | Do of ('a -> 'a) | Right

let example = Tape { left = [ 3; 2; 1 ]; head = 4; right = [ 5; 6 ] }

(* 2. a) *)

let obrni list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> pomozna (gl :: acc) rep
  in
  pomozna [] list

let map (Tape tape) f =
  let rec map_pomozna acc funct = function
    | [] -> obrni acc
    | gl :: rep -> map_pomozna (funct gl :: acc) funct rep
  in
  let (Tape { left; head; right }) = Tape tape in
  let nov_lev = map_pomozna [] f left
  and nov_des = map_pomozna [] f right
  and nova_glava = f head in
  Tape { left = nov_lev; head = nova_glava; right = nov_des }

(* 2. b) *)

let izvedi (Tape tape) = function
  | Left -> (
      match obrni tape.left with
      | [] -> None
      | zadnje :: ostali ->
          Some
            (Tape
               { left = ostali; head = zadnje; right = tape.head :: tape.right })
      )
  | Do f -> Some (Tape { tape with head = f tape.head })
  | Right -> (
      match tape.right with
      | [] -> None
      | prvi :: ostali ->
          Some
            (Tape
               { left = tape.left @ [ tape.head ]; head = prvi; right = ostali })
      )

(* 2. c) *)

let rec izvedi_ukaze tape = function
  | [] -> tape
  | ukaz :: rep -> (
      match izvedi tape ukaz with None -> tape | Some x -> izvedi_ukaze x rep )

(* 2. d) *)

let naberi_in_pretvori tape list =
  let rec generator_parov acc current_tape cmd_list =
    let (Tape { left; head; right }) = current_tape in
    match cmd_list with
    | [] -> obrni acc
    | prvi :: ostali -> (
        match izvedi current_tape prvi with
        | None -> obrni acc
        | Some x -> (
            match prvi with
            | Do f -> generator_parov ((head, f head) :: acc) x ostali
            | _ -> generator_parov acc x ostali ) )
  in
  let pari = generator_parov [] tape list
  and nov_trak = izvedi_ukaze tape list in
  (pari, nov_trak)

(* 2. e) *)

let pripravi_ukaze tape f =
  let (Tape { left; head; right }) = tape in
  let desni = List.length right in
  let rec premikanje_glave_levo acc current_tape =
    (*Glavo premikamo v levo, dokler ni glava skrajni levi element.
      hkrati shranjujemo zaporedne ukaze Left. *)
    match izvedi current_tape Left with
    | None -> acc
    | Some x -> premikanje_glave_levo (Left :: acc) x
  in
  let rec izvajanje acc current_tape =
    (*f izvedemo na vseh elementih traku.*)
    match izvedi current_tape (Do f) with
    | None -> obrni acc
    | Some x -> (
        match izvedi x Right with
        | Some y -> izvajanje (Right :: Do f :: acc) y
        | _ -> obrni acc )
  in
  let rec nazaj_na_pravo_glavo acc razdalja =
    (*Glavo premakne na originalno mesto.*)
    if razdalja > 0 then nazaj_na_pravo_glavo (Left :: acc) (razdalja - 1)
    else acc
  in
  (*združimo rezultate*)
  let premik_v_levo = premikanje_glave_levo [] tape in
  let izvajanje_f = izvajanje [] (izvedi_ukaze tape premik_v_levo) in
  let glava_nazaj = nazaj_na_pravo_glavo [] desni in
  premik_v_levo @ izvajanje_f @ glava_nazaj
