type 'a operator = 'a -> 'a -> 'a

type 'a atom = Value of 'a | Operator of 'a operator

type 'a term = 'a atom list

type 'a result = Finished of 'a | Term of 'a term | Error

let plus = Operator ( + )

let krat = Operator ( * )

let deljeno = Operator ( / )

let primer : int term = [ Value 3; Value 4; plus; Value 5; deljeno ]

(* 2. a) *)

let primer1 : int term =
  [ Value 1; Value 2; plus; Value 4; krat; Value (-1); plus; Value 5; krat ]

let primer2 : float term = []

let napacni_primer = [ Value 1; plus; Value 2; Value 3; Value 4 ]

(* 2. b) *)

let korak _ = failwith "TODO"

(* 2. c) *)

let izvedi _ = failwith "TODO"

(* 2. d) *)

let valid _ = failwith "TODO"

(* 2. e) *)

let combine _ = failwith "TODO"
