(*Archivo pairs2.ml *)

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




(*EXPLICACIÓN DE PAIR_I*)
(* La función recursiva pair_i es lenta debido al uso de una búsqueda secuencial para encontrar el número deseado. En la implementación original, para cada valor de i, la función realiza una llamada a pair para verificar si es igual al valor buscado. Esta estrategia se vuelve ineficiente para valores grandes, ya que implica verificar cada número de manera secuencial hasta encontrar una coincidencia. La lentitud en la búsqueda secuencial se traduce en un aumento lineal en el tiempo de ejecución a medida que el valor de i crece *)



(*función pair_i' para mejorar los tiempos de búsqueda de pair_i*)
let pair_i' (x, y) =
  let rec find i =
    if pair i = (x, y) then i
    else if i < x + y then -1  (* El par no se encuentra en el rango esperado *)
    else find (i-1)
  in find (x + y)
  

