(*Archivo bintree.ml *)

(*árboles binarios, estructura jerárquica de datos donde cada nodo tiene como máximo dos hijos o ramas, la rama izquierda y la rama derecha *)
type 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree


(*función in_order que devuelve la lista de valores de los nodos recorridos de un árbol*)
let rec in_order = function
  | Empty -> []
  | Node (x, l, r) -> (in_order l) @ [x] @ (in_order r);;


(*función insert en la que si ord es una relación de orden y tree es un árbol binario de búsqueda, insert ord tree x será un árbol binario de búsqueda conteniendo los nodos que tenía tree más un único nodo adicional con valor x*)
let rec insert cmp tree x =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (value, left, right) ->
      if cmp x value then
        Node (value, insert cmp left x, right)
      else
        Node (value, left, insert cmp right x)
        
(*función bst, a partir de insert, de modo que si ord es una relación de orden, bst ord I será un árbol binario de búsqueda *)
let rec bst cmp lst =
  let rec insert_list tree = function
    | [] -> tree
    | h::t -> insert_list (insert cmp tree h) t
  in
  insert_list Empty lst


(*función qsort que ordena una lista según un criterio dado*)
let qsort cmp lst =
  let sorted_tree = bst cmp lst in
  in_order sorted_tree

