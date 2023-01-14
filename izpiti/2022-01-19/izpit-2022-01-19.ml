(*PRVA NALOGA*)

let sta pravokotna vektor1 vektor2 =
  let a1, b1, c1 = vektor1 and a2, b2, c2 = vektor2 in
  (a1 * a2) + (b1 * b2) + (c1 * c2) = 0

let postkompozicija f g x = g (f x)

let obrni list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> pomozna (gl :: acc) rep
  in
  pomozna [] list

let dopolni privzeta_vrednost list =
  let rec pomozna acc x = function
    | [] -> obrni acc
    | gl :: rep -> (
        match gl with
        | Some y -> pomozna (y :: acc) x rep
        | _ -> pomozna (x :: acc) x rep )
  in
  pomozna [] privzeta_vrednost list

let pretvori n seznam =
  (*seznam obrnemo, da štetje eksponentov ustreza indeksom*)
  let nov_seznam = obrni seznam in
  let rec pomozna acc i = function
    | [] -> 0
    | gl :: rep ->
        (*if it works it ain't stupid hvala*)
        let floatast_n = float_of_int n in
        let eksponent = int_of_float (floatast_n ** float_of_int i) in
        pomozna (acc + (gl * eksponent)) (i + 1) rep
  in
  pomozna 0 1 nov_seznam

(*DRUGA NALOGA*)

type 'a merkle = List | Vozlisce of 'a vozlisce

and 'a vozlisce = {
  levo : 'a merkle;
  podatek : 'a;
  desno : 'a merkle;
  zgostitev : int;
}

type 'a zgostitev = int -> 'a -> int -> int

let primer_h l p d = ((l * 3) + (p * 5) + (d * 7)) mod 11

let drevo : int merkle =
  Vozlisce
    {
      levo =
        Vozlisce
          {
            levo =
              Vozlisce
                { levo = List; podatek = 10; desno = List; zgostitev = 6 };
            podatek = 14;
            desno =
              Vozlisce
                { levo = List; podatek = 474; desno = List; zgostitev = 5 };
            zgostitev = 2;
          };
      podatek = 57;
      desno =
        Vozlisce
          {
            levo = List;
            podatek = 12;
            desno =
              Vozlisce
                { levo = List; podatek = 513; desno = List; zgostitev = 2 };
            zgostitev = 8;
          };
      zgostitev = 6;
    }
