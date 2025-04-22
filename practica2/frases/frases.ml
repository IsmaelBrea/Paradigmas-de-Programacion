(* Archivo: frases.ml *)

(*x -y;;*)
(* error de ejecución, OCaml no puede encontrar una definición válida para la variable x en ese contexto. *)

let x =1;;
(* val x : int = 1 *)

(* x-y;; *)
(* error de ejecución, OCaml no puede encontrar una definición válida para la variable y en ese contexto. *)

let y = 2;;
(* val y : int = 2 *)

x-y;;
(* int : -1 *)

let x=y in x-y;;
(* -: int = 0 *)
(* Esta expresión crea una nueva variable x que toma el valor de y (que es 2 en este momento) y luego calcula la resta de x e y. Dado que x se establece como igual a y, esto sería equivalente a 2 - 2, que da como resultado 0 *)

x-y;;
(* int = -1 *)
(* el valor de x en let in no se guarda, por tanto se utilza en que tenía antes asignado, 1-2 *)

(* z;; *)
(* Error de ejecución, z no tiene ningún valor almacenado, hacer declaración de z *)

let z = x + y;;
(* val z: int = 3 *)

z;;
(* z = 3 *)

let x=5;;
(* val x: int = 5 *)

x+y;;
(* int = 7 *)

z;;
(* z = 3 *)
(* el valor de z no varía *)


let y=5 in x+y;;
(* -: int = 10 *)

x+y;;
(*-: int = 7 *)
(* Se mantienen los últimos valores que se le asignaron a x e y fuera de los let in, x=5 e y=2 *)

let x = x + y in let y= x * y in x + y + z;;
(* val x: int = 24 *)
(* x = 2+5, y = 7 * 2 --> x=7, y=14,z=3 --> x+y+z = 24 *)

x + y + z;;
(*-: int = 10 *)
(* se mantienen los últimos valores fuera de los let in, x=5, y=2,z=3 *)

function x-> 2 * x;;
(* -: int -> int = <fun> *)

(function x-> 2 * x)(2+1);;
(* -: int = 6 *)

(function x-> 2 * x) 2 + 1;;
(* -: int = 5 *)

let f = function x -> 2 * x;;
(* val f: int -> int = <fun> *)

f;;
(* -: int -> int = <fun> *)
(* en este caso, al añadir antes el let, el resultado de la function si que se almacena *)

f(2+1);;
(*-: int = 6 *)

f 2 + 1 ;;
(*-: int = 5 *)

f x;;
(*-: int = 10 *)
(* Se lleva a cabo la function multiplicada por el último valor que recibe x que es 5 *)

let x = 100;;
(* val x: int = 100 *)

f x;;
(* int = 200 *)

let m = 1000;;
(* val m: int = 1000 *)

let g = function x -> x + m;;
(* -: val g: int -> int = <fun> *)

g;;
(*-: int -> int = <fun> *)

g 3;;
(*-: int = 1003 *)

(* g 3.0;;*)
(* error de tipo: operación con tipos de datos que no son compatibles(int y float) *)

let m = 7;;
(* -: val m : int = 7 *)

g 3;;
(*-: int = 1003 *) 
(* el valor de g captura el valor de m en su entorno léxico cuando se definió, por lo que sigue tomando a m como 1000 *)

let istrue = function true -> true;;  (* la función solo devuelve true cuando su argumento es true, pero no tienen ningún manejo para los valores que no sean true *)
(* val true: bool -> bool = <fun> *)
(*-: Salta warning, porque nos dice que hay casos de entrada que no están siendo manejados por nuestro código: el caso false *)


istrue;;
(* -: bool -> bool = <fun> *)

istrue(1<2);;
(*-: bool = true *)

istrue(2<1);;
(* Excepción, porque la función istrue está definida solo para manejar el caso en el que su argumento es true *)

(* istrue 0;, *)
(* Error de tipo, la función istrue es de tipo bool, no de tipo int *)


let iscero_v1 = function 0 -> true;; (* si el número es 0, nos dice que es true *)
(* val iscero_v1 : int -> bool = <fun> *)
(*Vuleve a saltar Warning, porque hay un caso de entrada que no está siendo manejado en nuestro código: el caso del 1 *)

iscero_v1 0;;
(* -: bool = true *)

(* iscero_v1 0.;; *)
(* Error de tipo : el argumento que recibe es de tipo float, y la función trabaja con enteros y devuelve un bool si el argumento es true *)


(* iscero_v1 1;; *)
(* Excepción, la función iscero_v1 solo está definida para manejar el caso en el que es true *)

let iscero_v2 = function 0 -> true | _ -> false ;;
(* val iscero_v2 : int -> bool = <fun> *)

iscero_v2 0;;
(*-: bool = true *)

iscero_v2 1;;
(*-: bool = false *)

(*-: iscero_v2 = 0.;; *)
(* Error de tipo, la función recibe un argumento de tipo float, y la función trabaja con enteros y devuelve un bool si el argumento es true *)

let all_to_true = function true -> true | false -> true;;
(* val all_to_true : bool -> bool = <fun> *)
(* la función hace que todos los argumentos devuelvan true aunque sean falsos *)

all_to_true (1<2);;
(*-: bool = true *)

all_to_true (2<1);;
(*-: bool = true *)

(* all_to_true 0;; *)
(* Error de tipo, el argumento que recibe es de tipo int  y la función solo trabaja con booleanos *)

let first_all_to_true = all_to_true;;
(* val first all_to_true : bool -> bool = <fun> *)

let all_to_true = function x -> true ;;
(* val all_to_true = 'a -> bool = <fun> *)
(* 'a- a representa un tipo genérico, es decir puede tomar un argumento de cualquier tipo(int,float...)*)
(* esta función SIEMPRE devuelve int *)

all_to_true (1<2);;
(*-: bool = true *)

all_to_true (2<1);;
(*-: bool = true *)

all_to_true 0;;
(*-: bool = true *)

(* first_all_to_true 0;; *)
(* Error de tipo: la función recibe un argumento de tipo int y la función trabaja solo con booleanos y devuelve siempre true *)





