(* Archivo ej43.ml *)


(*función reverse, que devuelve el entero invertido int-> int*)
let rec reverse n =
  if n < 10 then n else (n mod 10) * int_of_float (10. ** float_of_int (int_of_float (log10 (float_of_int n)))) + reverse (n / 10)
  

(*función palindromo, que devuelve true si es un palindromo, false si no lo es string->bool*)
let rec palindromo s =
  let len = String.length s in
  if len <= 1 then true
  else if s.[0] = s.[len - 1] then
    palindromo (String.sub s 1 (len - 2))
  else
    false

(*función mcd, que calcula el máximo común divisor de dos números*)
let rec mcd (x,y) = 
    if y = 0 then x 
    else mcd ((x mod y), y)

