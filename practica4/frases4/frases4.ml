(* Archivo: frases4.ml *)

let p = (1 + 1, asin 1.), true;;
(* val p : (int * float) * bool = ((2, 1.51.57079632679489656), true) *)

let (x,y), z = p;;
(* val x : int = 2
   val y : float = 1.57079632679489656
   val z : bool = true *)

let p1, p2 = p in let p11, _ = p1 in (p2, 2 * p11);; (* dividimos p en dos partes, p1 y p2. En  let p11,_ descomponemos la tupla en dos partes, la primera  toma el primer argumento de p1, y _ ignora el segundo. (p2, 2 * p11) crea una nueva tupla que contiene dos elementos, p2 que es true y 2 por p11, que es 2*2 = 4 *)
(* - : bool * int = (true, 4) *)

let f (x , y) = 2 * x + y;;
(* val f : int * int -> int = <fun> *)

let f2 x y z = x + 2 * y + 3 * z;;
(* val f2: int -> int -> int -> int = <fun> *) 


let g x y z = x (y, z);;
(* val g : ('a * 'b -> 'c) -> ('a -> 'b -> 'c) = <fun> *)


g fst 1 "hola";;
(* - : int = 1 *)

g snd fst true;;
(* - : bool = true *)

g f 2 3;;
(* - : int = 7 *)

g (function (f, x) -> f (f x)) (function x-> x * x) 3;;
(* -: int = 81 *)

let x, y, z =1, 2, 3;;
(* val x: int = 1 *
   val y: int = 2 
   val z: int = 3 *)
   
   
f2 x y z;;
(* -: int = 14 *)

let x, y , z = y,z, x in f2 x y z;;
(* -: int = 11 *)

f2 x y z;;
(* -: int = 14 *)

let swap (x,y) = y,x;;
(* val swap : 'a * 'b -> 'b * 'a = <fun> *)

let p = 1, 2;;
(* val p : int * int = (1, 2) *)

f p;;
(* -: int = 4 *)

let p = swap p in f p;;
(*-: int = 5 *)

f p;;
(* -: int = 4 *)








