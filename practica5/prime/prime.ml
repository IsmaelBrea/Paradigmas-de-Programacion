(*Archivo: prime.ml *)

(* calcula si n es primo int-> bool = <fun> *)
let is_prime n =
let rec check_from i =
i >= n ||
(n mod i <> 0 && check_from (i+1))
in check_from 2;;

(* calcula el siguiente primo a n *)
let rec next_prime n= 
 if is_prime (n+1) then n+1
 else next_prime (n+1)
 
 (* calcula el anterior primo a n *)
let rec last_prime_to n= 
 if is_prime (n) then n
 else last_prime_to (n-1)
 
 (* calcula si n es primo con una versión más eficiente que is_prime*)
 let rec is_prime2 n = let rec check i = (float_of_int i) >= (sqrt(float_of_int n)) +. 1.0 || (n mod i <> 0 && check(i+1)) in check 2
 
 
