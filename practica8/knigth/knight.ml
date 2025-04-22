(*Archivo kings.ml *)


(*El problema del caballo: teniendo en cuenta una cuadrícula de nxn casillas y un caballo de ajedrez colocado en cualquiera posición (x,y), el caballo pase por todas las casillas y una sola vez*)


(*función tour, que permita realizar un recorrido de saltos de caballo en un tablero mxn. La funcion debe evitar pasar por las casillas especificadas en la lista de obstaculos, partiendo desde ini hasta fin. La ruta deber ser tal que no pase dos veces por la misma casilla y no debe salirse del tablero. Si no existe el recorrido devuelve la excepción Not_found*)
exception Not_found

let tour m n obstaculos ini fin =
  let is_safe x y visited =
    x >= 0 && y >= 0 && x < m && y < n &&
    not (List.mem (x, y) visited) &&
    not (List.mem (x, y) obstaculos)
  in

  let possible_moves x y =
    [(x + 1, y + 2); (x + 1, y - 2); (x - 1, y + 2); (x - 1, y - 2);
     (x + 2, y + 1); (x + 2, y - 1); (x - 2, y + 1); (x - 2, y - 1)]
  in

  let rec tour_helper visited current end_pos =
    let x, y = current in
    if current = end_pos then
      visited @ [current]
    else
      let possible = possible_moves x y in
      let valid_moves = List.filter (fun (a, b) -> is_safe a b visited) possible in
      match valid_moves with
      | [] -> raise Not_found
      | h :: t ->
          try
            tour_helper (visited @ [current]) h end_pos
          with Not_found ->
            if t = [] then
              raise Not_found
            else
              tour_helper visited (List.hd t) end_pos
  in

  try
    tour_helper [] ini fin
  with Not_found -> raise Not_found


  
  
  
(*función min_tour, hace lo mismo que la anterior función, pero en vez de devolver un camino cualquiera, devuelve el camino más corto*) 
exception Not_found

let min_tour m n obstacles ini fin =
  let is_safe x y visited =
    x >= 0 && y >= 0 && x < m && y < n &&
    not (List.mem (x, y) visited) &&
    not (List.mem (x, y) obstacles)
  in

  let possible_moves x y =
    [(x + 1, y + 2); (x + 1, y - 2); (x - 1, y + 2); (x - 1, y - 2);
     (x + 2, y + 1); (x + 2, y - 1); (x - 2, y + 1); (x - 2, y - 1)]
  in

  let rec bfs_distance queue visited =
    match queue with
    | [] -> raise Not_found
    | (current_x, current_y, path) :: rest ->
        if (current_x, current_y) = fin then
          List.rev ((current_x, current_y) :: path)
        else
          let possible = possible_moves current_x current_y in
          let valid_moves = List.filter (fun (a, b) -> is_safe a b (visited @ [(current_x, current_y)])) possible in
          let new_paths = List.map (fun (x, y) -> (x, y, (current_x, current_y) :: path)) valid_moves in
          let new_queue = rest @ new_paths in
          bfs_distance new_queue ((current_x, current_y) :: visited)
  in

  try
    bfs_distance [(fst ini, snd ini, [])] []
  with Not_found -> raise Not_found

  
  
  
(*función min_tour4D,lo mismo que min_tour, pero considerando movimientos en un espacio tridimensional expandido *)
exception Not_found

let min_tour4D m n obstacles ini fin =
  let is_safe x y =
    x >= 0 && y >= 0 && x < m && y < n &&
    not (List.mem (x, y) obstacles)
  in

  let possible_moves x y =
    [(x + 1, y + 2); (x + 1, y - 2); (x - 1, y + 2); (x - 1, y - 2);
     (x + 2, y + 1); (x + 2, y - 1); (x - 2, y + 1); (x - 2, y - 1)]
  in

  let rec bfs_distance queue visited =
    match queue with
    | [] -> raise Not_found
    | (x, y, path) :: rest ->
        if (x, y) = fin then
          List.rev ((x, y) :: path)
        else
          let new_positions = possible_moves x y in
          let valid_moves =
            List.filter
              (fun (a, b) ->
                let new_x = (a + m) mod m in
                let new_y = (b + n) mod n in
                is_safe new_x new_y && not (List.mem (new_x, new_y) visited) && not (List.mem (new_x, new_y) path))
              new_positions
          in
          let new_queue = rest @ List.map (fun (a, b) -> (a, b, (x, y) :: path)) valid_moves in
          bfs_distance new_queue ((x, y) :: visited)
  in

  try
    bfs_distance [(fst ini, snd ini, [])] []
  with Not_found-> raise Not_found
