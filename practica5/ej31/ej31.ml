(* Archivo: ej31.ml *)

(* definición de g en g1 sin usar && ni || *)
let g1 n = if (n >= 0) then (if (n mod 2 = 0) then true else false) else (if (n mod 2 = -1) then true else false);;

(* definición de g en g2 sin usar && ni || ni if-then-else *)
let g2 n =
  match n with
  | n when n >= 0 ->
    (match n mod 2 with
    | 0 -> true
    | _ -> false)
  | _ -> (match n mod 2 with
          | -1 -> true
          | _ -> false)


  
