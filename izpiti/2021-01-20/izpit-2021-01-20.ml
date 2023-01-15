(*PRVA NALOGA*)

let razlika x y = (x * y) - (x + y)

let zlepi_para par1 par2 =
  let a, b = par1 and c, d = par2 in
  (a, b, c, d)

let trojica_graficno x y z =
  let pomozna = function None -> "-" | Some a -> string_of_int a in
  "(" ^ pomozna x ^ ", " ^ pomozna y ^ ", " ^ pomozna z ^ ")"

let nedeljivo_do x n =
  let rec pomozna acc_i m =
    (*če seljimo z ena, potem ni deljivo z večjimi*)
    if acc_i < 2 then true
      (*sicer preverimo deljivost z acc_i in izvedemo rekurzijo za predhodnjika*)
    else if m mod acc_i = 0 then false
    else pomozna (acc_i - 1) m
  in
  pomozna n x

let obrni list =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> pomozna (gl :: acc) rep
  in
  pomozna [] list

let razcepi_pri_None seznam =
  let rec pomozna acc trenutni_podseznamcek = function
    | [] -> obrni (trenutni_podseznamcek :: acc) (*dodamo še zadnji podseznam*)
    | gl :: rep -> (
        match gl with
        (*če pridemo do None bomo trenutni podseznamček kot element priključili akumulatorji rezultata*)
        | None -> pomozna (obrni trenutni_podseznamcek :: acc) [] rep
        (*sicer bomo glavo priključili trenutnemu podseznamčku in nadaljevali do None*)
        | Some x -> pomozna acc (x :: trenutni_podseznamcek) rep )
  in

  pomozna [] [] seznam

(*DRUGA NALOGA*)

type 'a kuhinjski_element = Ponev of 'a | Lonec of 'a * 'a | Omara of 'a list

let kuhinja =
  [
    Ponev "tuna";
    Lonec ("brokoli", "mango");
    Omara [ "sir"; "toast"; "sok"; "ragu" ];
  ]

let prestej seznam =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> (
        match gl with
        | Ponev x -> pomozna (acc + 1) rep
        | Lonec (x, y) -> pomozna (acc + 2) rep
        | Omara list ->
            let dolzina = List.length list in
            pomozna (acc + dolzina) rep )
  in
  pomozna 0 seznam

let pretvori f = function
  | Ponev x -> Ponev (f x)
  | Lonec (x, y) -> Lonec (f x, f y)
  | Omara list -> Omara (List.map f list)

let pospravi seznam =
  let rec pomozna acc = function
    | [] -> acc
    | gl :: rep -> (
        match gl with
        | Ponev x -> pomozna (x :: acc) rep
        | Lonec (z, y) -> pomozna (y :: z :: acc) rep
        | Omara list ->
            let rec zdruzi_seznama nov_seznam = function
              | [] -> nov_seznam
              | gl :: rep -> zdruzi_seznama (gl :: nov_seznam) rep
            in
            pomozna (zdruzi_seznama acc list) rep )
  in
  Omara (pomozna [] seznam)

let oceni cenilka kuhinja =
  let pomnozi_ceno = function
    | Ponev x -> x
    | Lonec (x, y) -> 3 * (x + y)
    | Omara xs -> 5 * List.fold_left ( + ) 0 xs
  in
  kuhinja
  |> List.map (pretvori cenilka)
  |> List.map pomnozi_ceno |> List.fold_left ( + ) 0

(*Popravki:
  Kuhinja je seznam kuhinjskih elementov. 
  Vsebino elementov želimo zamenjati z vrednostjo cenilke zanje - dobimo kuhinjo int kuhinjskih elementov.
  "pretvori cenilka" deluje na posameznem elementu, mi pa imamo seznam - List.map bo vzela to funkcijo in jo izvedla na vseh elementih seznama. 
  Funkcija oceni ni rekurzivna.
  *)
