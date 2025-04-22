(* Archivo bintree_opt.ml *)

type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree



let rec is_bst cmp tree =
  let rec is_bst_helper node lower_bound upper_bound =
    match node with
    | Empty -> true
    | Node (value, left, right) ->
      cmp value lower_bound && cmp upper_bound value &&
      is_bst_helper left lower_bound value &&
      is_bst_helper right value upper_bound
  in
  is_bst_helper tree min_int max_int


(* función bfs *)
let bfs tree =
  let rec process_queue queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: rest -> process_queue rest acc
    | Node (value, left, right) :: rest ->
        let updated_acc = value :: acc in
        let updated_queue = rest @ [left; right] in
        process_queue updated_queue updated_acc
  in
  match tree with
  | Empty -> []
  | _ -> process_queue [tree] []

  


(* Función bfs' *)
let bfs' tree =
  let rec level_traversal queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: tl -> level_traversal tl acc
    | Node (value, left, right) :: tl ->
        let updated_acc = value :: acc in
        let updated_queue = tl @ [left; right] in
        level_traversal updated_queue updated_acc
  in
  match tree with
  | Empty -> []
  | _ -> level_traversal [tree] []



(* función perfecto *)
let rec perfecto tree =
  let rec count_nodes_height tree =
    match tree with
    | Empty -> (0, 0)
    | Node (_, left, right) ->
        let (left_nodes, left_height) = count_nodes_height left in
        let (right_nodes, right_height) = count_nodes_height right in
        let total_nodes = 1 + left_nodes + right_nodes in
        let current_height = 1 + max left_height right_height in
        (total_nodes, current_height)
  in
  let (node_count, tree_height) = count_nodes_height tree in
  let max_node_count = int_of_float (2. ** float_of_int tree_height) - 1 in
  node_count = max_node_count




(* función casi_completo *)
let casi_completo tree =
  let rec verificar_ultimo_nivel queue =
    match queue with
    | [] -> true
    | Empty :: _ -> false (* Se encuentra un nodo vacío antes de finalizar *)
    | Node (_, left, right) :: rest ->
        (* Añadir los hijos al final de la cola *)
        let new_queue = rest @ [left; right] in
        verificar_ultimo_nivel new_queue
  in
  let rec bfs queue =
    match queue with
    | [] -> true  (* Todos los nodos del último nivel están a la izquierda *)
    | _ -> verificar_ultimo_nivel queue
  in
  match tree with
  | Empty -> true (* Un árbol vacío se considera casi completo *)
  | _ -> bfs [tree]



