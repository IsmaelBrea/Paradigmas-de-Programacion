(* Archivo ej42.ml *)
(*Redefinimos las funciones de min, max, fst y snd*)

let min a b = if a<b then a else b;;

let max a b = if a>b then a else b;;

let fst (x, _) = x;;

let snd (_,y) = y;;


