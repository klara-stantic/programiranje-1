(*----------------------------------------------------------------------------*]
 PRVA NALOGA
[*----------------------------------------------------------------------------*)

let rec naloga1a element = function
  | [] -> element
  | f1 :: ostale -> naloga1a (f1 element) ostale

let rec naloga1b element = function
  | [] -> element
  | f1 :: ostale ->
      if f1 element > element then naloga1b (f1 element) ostale
      else naloga1b element ostale


(*----------------------------------------------------------------------------*]
 DRUGA NALOGA
[*----------------------------------------------------------------------------*)



