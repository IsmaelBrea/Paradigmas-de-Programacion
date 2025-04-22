(*Archivo queens.ml*)

(*Problema de las reinas: es un desafío de colocación en un tablero de ajedrez de tamaño nxn, de manera que ningun reina puede atacar a otra, es decir,ninguna reina debe estar en la misma fila, columna o diagonal que otra reina *)


(*función queens: int -> (int * int) list list, que devuelve para cada n el número de posiciones (coordenadas) en las que se pueden colocar las reinas en un tablero de tamaño nxn*)
let queens n =
  let rec is_safe row col queens =
    let is_safe_row r c = List.for_all (fun (x, y) -> x <> r && y <> c) queens in
    let is_safe_diag r c = List.for_all (fun (x, y) -> abs (x - r) <> abs (y - c)) queens in
    is_safe_row row col && is_safe_diag row col
  in
  
  let rec add_queen row queens =
    if row > n then [queens]
    else
      let try_queen col =
        if is_safe row col queens then
          let new_queens = queens @ [(row, col)] in
          add_queen (row + 1) new_queens
        else []
      in
      List.concat (List.map try_queen (List.init n (fun x -> x + 1)))
  in
  
  if n <= 0 then [[]]
  else add_queen 1 []



(*función is_queens_sol -> (int * int) list -> bool tal que n sol indique si la lista sol es una solución válida para el problema de las n reinas. No se puede usar la función anterior queens *)
let rec is_queens_sol n sol =
  let rec conflict x y = function
    | [] -> false
    | (a, b)::t ->
        y = b || x - y = a - b || x + y = a + b || conflict x y t
  in
  let rec check_positions = function
    | [] -> true
    | (x, y)::t ->
        if x < 1 || x > n || y < 1 || y > n || conflict x y t then
          false
        else
          check_positions t
  in
  match sol with
  | [] -> n = 0
  | _ -> List.length sol = n && check_positions sol




