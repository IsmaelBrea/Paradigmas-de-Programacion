(* Archivo : if_then_else.ml *)

(* let x=2 in let y=3 in *)
(function true -> "first is greater" | false -> "second is greater")(x>y);;  (* si x es mayor que y devuelve el string "first is greater" y si no devuelve "second is greater" *)


(* let x=(-3) in *)
(function true -> x | false -> -x) (x>0);; (* si x es mayor que 0 devuelve x, si no devuelve -x *)  


(* let x = 3 in let y = (-2) in *)
(function true -> x | false -> (function true -> y | false -> 0) (y > 0)) (x > 0);; (* si x es mayor que 0 devuelve x, si no comprueba si y es mayor que 0 y si lo es devuelve y. Si ni x ni y son mayores que 0, devuelve 0 *)
 
 
(* let x = (-2) in let y = 5 in let z = 3 in *)
 (function true -> (function true -> x | false -> z)(x>z) | false -> (function true -> y | false -> z) (y>z)) (x>y);;
(* si x es mayor que y evaluamos la primera rama (true). Aquí si x es mayor que z devuelve x y si no devuelve z. Si x es menor que z se comprueba la segunda rama(false). Aquí si y es mayor que z devuelve y si no devuelve z *)
 
 
