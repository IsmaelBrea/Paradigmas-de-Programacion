(* Archivo : def2_a.ml *)

(* Una función p : float -> float que haga corresponder a cada número no negativo
el perı́metro de la circunferencia que tenga como radio ese número (no importa lo
que suceda con los negativos *)
let p r = let pi = 2.0 *. asin 1.0 in if r > 0.0 then 2.0 *. pi *. r else 0.0;;


(* Una función area : float -> float que haga corresponder a cada número no
negativo el área del cı́rculo que tenga como radio ese número (no importa lo que
suceda con los negativos *)
let area x= let pi = 2.0 *. asin 1.0 in if x>0. then pi *. x *. x else 0.0;;


(* Una función absf : float -> float que haga correponder a cada número su valor
absoluto *)

let absf  (x:float) = abs_float x;;


(* Una función even : int -> bool de modo que, al aplicarla a cualquier entero,
el resultado que devuelva indique si el entero es par. Intente que la definción sea
lo más concisa posible (pista: intente evitar el uso de expresiones if-then-else
innecesarias *)

let even x =  x mod 2 = 0;; (*si el resto de x es 2 devuelve true, sino devuelve false *)


(* Una función next3 : int -> int que haga corresponder a cada entero el menor
múltiplo de 3 que sea mayor o igual que él *)
let next3 x = if(x mod 3 = 0) then x else x + (3 - (x mod 3));; 
(* si el resto del número entre 3 es 0, el menor número múltiplo de 3 mayor o igual que él es el mismo. Si el resto de dividir entre 3 es distinto de 0, restamos 3 - el resto y le sumamos el número entero, que dará el primer número entero múltiplo de 3 mayor que x *)

(* Una función is_a_letter : char -> bool que devuelva true en los caracteres de la ’a’ a la ’z’ (tanto mayúsculas como minúsculas) y false en los demás. A estos efectos, sólo consideraremos como ”letras” los caracteres del alfabeto inglés (es decir, quedan excluidas la 'n’, la ’ç’, las letras con tilde, etc *)
let is_a_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');;is


(* Y por último, redefina la función string of bool : bool -> string, de modo que
devuelva (adecuadamente) los valores "verdadero" o "falso" (puede hacerse una
definición por casos o utilizarse una expresión if-then-else *)
let string_of_bool x = if x then "Verdadero" else "Falso";;
