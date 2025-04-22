(*Archivo collatz.ml *)

(* conjetura de Collatz int->int*)
let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

(* Función orbit que genera el camino desde un número hasta 1 usando la función Collatz como cadena  int-> string*)
let rec orbit n =
  if n = 1 then
    "1"
  else
    string_of_int n ^ ", " ^ orbit (f n)

(* Función length que indique el número de pasos necesarios para llegar a la órbita int-> int*)
let rec length n =
  if n=1 then 
    0
  else 
    1 + length (f n)

(* Función top que calcule el mayor número de la órbita int-> int*)
let rec top n =
  if n=1 then 
    0
  else 
    max n (top (f n))
  
(* Función length'n'top sin usar ni length ni top que indique la longitud de su órbita y su altura máxima órbita int-> int * int*)  
let rec length'n'top n =
  let rec aux n len top =
    if n = 1 then
      (len, max top 1)
    else
      aux (f n) (len + 1) (max top n)
  in
  aux n 0 1
  
  
(* Función longest_in que devuelva el menor valor del intervalo int-> int -> int * int *)  
let rec longest_in m n =
  let rec aux i (best_val, best_len) =
    if i <= m then
      (best_val, best_len)
    else
      let (j, lj) = aux (i - 1) (best_val, best_len) in
      let li = length i in
      if lj >= li then
        (j, lj)
      else
        (i, li)
  in
  aux n (m, length m)
 
   
(* Función highest_in que devuelva el menor valor del intervalo int-> int -> int * int *)  
let highest_in m n =
  let rec aux i (min_val, min_top) =
    if i <= m then
      (m, top m)
    else
      let (j, t_j) = aux (i - 1) (min_val, min_top) in
      let t_i = top i in
      if t_i > t_j then
        (i, t_i)
      else if t_i < min_top then
        (i, t_i)
      else
        (j, t_j)
  in
  aux n (m, top m)







 
 
  
  
