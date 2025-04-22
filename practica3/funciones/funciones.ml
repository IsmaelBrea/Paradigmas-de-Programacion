(* Archivo funciones.ml *)

let rec factorial = function 0 -> 1 | n -> n * factorial (n-1);;
(* -: val factorial: int -> int  = <fun> *)

factorial 0 + factorial 1 + factorial 2;;
(* -: int = 4 *)

factorial 10;;
(* int = 3628800 *)

(* factorial 1000 ;;*)  (* Da un número muy grande *)
(* int = 0 *) 

(* factorial (-1);; *)
(* Error de desbordamiento de la pila debido a la recursión infinita *)




(* DEFINICIÓN DE FUNCIONES *)


let rec sumto = function 0-> 0 | n -> n + sumto(n-1);; (* sumto n devuelve la suma de los n primeros números naturales *)

let rec exp10 n = if n = 0 then 1 else if n >= 0 then 10 * exp10 (n - 1) else 0;; (* exp10 devuelve para cualquier n>= 10  el valor de 10 elevado a n *)

let rec num_cifras n = if n = 0 then 1 else if n < 0 then num_cifras (-n) else 1 + num_cifras (n / 10);; (* num_cifras devuelve el número de cifras que tiene un número *)

let rec sum_cifras n = if n = 0 then 0 else if n<0 then sum_cifras (-n) else (n mod 10) + sum_cifras (n / 10);; (* sum_cifras devuelve la suma de las cifras del número n *)






