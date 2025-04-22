(* Archivo: def.ml *)


(* Un valor v de tipo int a partir de una expresión que contenga al menos una función
predefinida *)
let v  = int_of_char 'c';;


(* Un valor w de tipo float a partir de una expresión que contenga al menos 4
operadores infijos *)
let w = (2.0 +. 3.2 *. 4.0 -. 2.5) /. 2.0 ;;


(* Un valor x de tipo char a partir de una expresión que contenga una frase
if-then-else *)
let x = if 10>7 then 'X' else 'Y';;


(* Un valor y de tipo bool a partir de una expresión que contenga una o más funciones
u operadores *)
let y = let a=6 in let b = -5 in (abs (a) < abs (b)) && (a mod 2 = 0) ;;

(*Un valor z de tipo string a partir de una expresión que contenga una sub-expresión
de tipo int*)
let z = let a= int_of_char 'A' in let b = int_of_string "65" in if a!=b then "Las letras son distintas" else "Las letras son iguales";;



