(* Archivo: expr.ml *)

();;  
(* -: unit=() *)


2 + 5 * 3;;
(* -: int = 17 *)


1.25 *. 2.0;;
(* -: float = 2.5 *)


(* 2 - 2.0;; *)
(* -: Error de tipo: operación con tipos de datos que no son compatibles*)
(* Para que estuviese bien después de la resta hay que poner un punto para que sea float, o también podríamos convertir el 2 a float (float_of_int)  *)


(* 3.0 + 2.0;; *)
(* -:  Error de tipo: operación incorrecta entre tipos de datos *)
(* Para que estuviese bien después de la suma hay que poner un punto para que sea float *)



5 / 3;;
(* -: int = 1 *)
(* Solo sale la parte entera de la operación*)



5 mod 3;; (* mod = resto *)
(* -: int = 2 *)

2.0 *. 3.0 ** 2.0;;  (*primero se evalua la potencia (solo float) y luego el producto *)
(* - : float = 18 *) 



2.0 ** 3.0 ** 2.0;;  (* Las expresiones exponenciales se evaluan de derecha a izquierda*)
(* - : float = 512 *) 



sqrt;;
(* - : float -> float = <fun>*) 



(* sqrt 4 *) ;;
(* -: Error sintáctico: el código no cumple con la gramática del lenguaje *)
(* para que diese float = 2, habria que poner sqrt 4.;; *)



int_of_float;;
(* - : float-> int = <fun> *) 



float_of_int;;
(* - : int -> float = <fun> *) 



3.0 = float_of_int 3;; (* Esto dice que 3.0 en float es 3 en int, por tanto es true (bool) *)
(* - : bool = true *) 



(* int_of_float -2.9;; *)  
(* - : Error sintáctico: el código no cumple con la gramática del lenguaje*)
(* Da este error porque el número al ser negativo tiene que ir entre parentesis y convertirá un float a int (-2) *)
 
 
 
int_of_float 2.1 + int_of_float(-2.9);; (* los numeros positivos no tienen porque ir entre paréntesis*)
(*- : int = 0 *)
(* El resultado es un int porque pasamos de float a int, y da 0 porque sumamos 2 y -2 *)



truncate ;;  
(* - : float -> int = <fun> *)



truncate 2.1 + truncate (-2.9);;
(* - : int = 0 *)

floor;; 
*)
(*- : float -> float = <fun> *)



floor 2.1 +. floor (-2.9);; (* el 2.1 se aproxima a 2 y el -2.9 a -3 (porque hacia abajo la aproximació es -3, que es menor que -2.9), y la suma de 2 + (-3) es -1 *)
(* - : float = -1 *)



ceil;;
(*-: float -> float = <fun> *)



ceil 2.1 +. ceil(-2.9);;
(* -: float = 1 *)



int_of_char;;
(* -: char -> int = <fun> *)



int_of_char 'A';;  (* devuelve el numero de A equivalente en la tabla ASCII*)
(* -: int = 65 *)  



char_of_int ;; (*devuelve el caracter  equivalente a su número en ASCII *)
(* -: int -> char = <fun> *)



char_of_int 66;;
(* -: char = 'B' *)



Char.code;;  
(* -: char -> int = <fun> *)



Char.code 'B';;
(* -: int = 66 *)



Char.chr;; 
(* -: int -> char = <fun> *)



Char.chr 67;;
(* -: char = 'C' *)



'\067';;  (* devulve el caracter equivalente a su número en ASCII *)
(* -: char = 'C' *)


Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');;  (* 97-65+77 *)
(* -: char = 'm' *)



Char.lowercase_ascii;;  (* Lower convierte de minúsculas a mayúsculas *)
(* -: char -> char = <fun> *)



Char.lowercase_ascii 'M';;
(* -: char = 'm' *)



Char.uppercase_ascii;;  (* Upper convierte de mayúsculas a minúsculas *)
(* -: char -> char = <fun> *)



Char.uppercase_ascii 'm';;
(* -: char = 'M' *)



"this is a string";;  (* escribe por pantalla la cadena de texto *)
(* -: string = this is a string *)



String.length;;  (* devuelve la longitud de la cadena *)
(* -: string -> int = <fun> *)



String.length "longitud" ;;
(* -: int = 8 *)



(* "1999" + "1" ;;  *)
(* Error de tipo: operaciones con tipos de datos que no son compatibles *)



(* "1999" ^ "1" ;;  *)
(* -: string = "19991" *)



int_of_string;;
(* string -> int = <fun> *)



int_of_string "1999" + 1;;  (*convierte el string 1999 a int y se lo suma a 1*)
(*-: int = 2000*)


"\065\066";;  (*representa caracteres especiales en una cadena *)
(*:- string: "AB" *)



string_of_int;;
(* -: int -> string = <fun> *)



string_of_int 010;;  (* convierte el entero 010 a cadena "010" *)
(* -: string = "10"*)



not true;;
(*-: bool = false *)



true && false ;;  (*solo devuelve true si ambos operadores son true*)
(*-: bool = false *)



true || false ;;  (* devuelve true si al menos uno de los operadores es true*)
(*-: bool = true *)



(1<2) = false;;
(* -: bool = false *)



"1" < "2";;  (* compara como cadena de caracteres *)
(*-: bool = true *)



2<12;; (* comparación numérica *)
(* -: bool = true *)



"2"<"12";;  (*compara como cadena de caracteres, como el 2 es el mayor que el primer 1, es falso *)
(* bool = false *)



 "uno"< "dos";;
 (* -: bool = false *)


 
if 3=4 then 0 else 4;; (* si 3 es igual a 4 devuelve 0 como número entero, si no devuelve 4 como número entero *)
 (* - : int = 4 *)
 
 
 
if 3=4 then "0" else "4";; (* si 3 es igual a 4 devuelve 0 como cadena de texto, si no devuelve 4 como cadena de texto *)
(* - : string = 4 *)
 
 
 
(* if 3=4 then 0 else "4"*);;  (* no se pueden mezlar tipos *)
(* -: Error de tipo, mezcla int con strings *)
 
 
 
 (if 3<5 then 8 else 10) + 4;; 
 (* int = 12 *)
 

