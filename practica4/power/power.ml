(*Archivo: power.ml *) 

(*val power : int -> int -> int = <fun>*)
let rec power x n =
  if n = 0 then 1
  else x * power x (n - 1)

(*val power' : int -> int -> int = <fun>*)
let rec power' x n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let half_pow = power' x (n / 2) in
    half_pow * half_pow
  else
    x * power' x (n - 1)

(* La función power' es más eficiente, ya que reduce n a la mitad en cada recursión.
   Esto resulta en un menor número de operaciones, lo que es especialmente beneficioso
   para números grandes que podrían exceder el rango de enteros. *)

(* val powerf: float -> int -> float *)
let rec powerf x n =
  if n = 0 then 1.0
  else if n mod 2 = 0 then
    let half_pow = powerf (x *. x) (n / 2) in
    half_pow
  else
    x *. powerf (x *. x) (n / 2)

