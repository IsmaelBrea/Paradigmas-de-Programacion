(*Archivo pairs.ml *)

(*función next , que devuelve el siguinte par correspodiente al introducido según la secuencia*)
let next (x, y) =
  if (x + y) mod 2 = 0 then  (* Diagonales pares: ascendente *)
    if x = 1 then
      (x, y + 1)  (* Movimiento hacia arriba en la primera columna *)
    else
      (x - 1, y + 1)  (* Movimiento diagonal hacia arriba en otras columnas *)
  else  (* Diagonales impares: descendente *)
    if y = 1 then
      (x + 1, y)  (* Movimiento hacia la derecha en la primera fila *)
    else
      (x + 1, y - 1)



(*función steps_from, devuelve el par al que se llega después de avanzar n pasos partiendo de un par *)
let rec steps_from (x, y) n =
  if n = 0 then
    (x, y)
  else
    steps_from (next (x, y)) (n - 1)
    
    


(*función pair, que devuelve el par del paso en el que se está*)
let rec pair n =
  let next_position (x, y) =
    if (x + y) mod 2 = 0 then
      if x = 1 then
        (x, y + 1)
      else
        (x - 1, y + 1)
    else
      if y = 1 then
        (x + 1, y)
      else
        (x + 1, y - 1)
  in
  if n = 1 then
    (1, 1)
  else
    next_position (pair (n - 1))


