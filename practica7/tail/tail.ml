(*Archivo tail.ml*)

(*Redefinir las funciones de manera recursiva terminal*)

(*función to0from, lista descendente de n a 0*)
let to0from n =
  let rec aux acc counter =
    if counter < 0 then List.rev acc
    else aux (counter :: acc) (counter - 1)
  in
  aux [] n

(*función fromto, que crea una lista de números consecutivos desde n hasta m*)
let fromto m n =
  let rec fromto_aux acc m' n' =
    if m' > n' then List.rev acc
    else fromto_aux (m' :: acc) (m' + 1) n'
  in
  fromto_aux [] m n;;

(*función remove, que elimina un elemento de una lista*)
let remove x lst =
  let rec remove_tail acc = function
    | [] -> List.rev acc
    | hd::tl ->
        if hd = x then
          List.rev_append acc tl  (* Elimina el elemento y concatena la cola *)
        else
          remove_tail (hd::acc) tl  (* Conserva el elemento actual y sigue buscando *)
  in
  remove_tail [] lst


(*función compress que elimina elementos consecutivos duplicados en una lista.
   Versión recursiva terminal*)
let rec compress = function
  | h1::h2::t ->
      if h1 = h2 then
        compress (h2::t)
      else
        h1 :: compress (h2::t)
  | l -> l

(*función append' que concatena dos listas *)
let append' lst1 lst2 =
  let rec rev_append_tail acc lst =
    match lst with
    | [] -> acc
    | hd :: tl -> rev_append_tail (hd :: acc) tl
  in
  rev_append_tail lst2 (List.rev lst1)


(*función map' que aplica una función a cada elemento de una lista*)
let map' f lst =
  let rec map_helper acc = function
    | [] -> List.rev acc
    | h::t -> map_helper (f h :: acc) t
  in
  map_helper [] lst



(*función fold_right' que aplica una función binaria a cada elemento de una lista, comenzando desde el final de la lista*)
let fold_right' f lst init =
  let rec fold_right_helper acc = function
    | [] -> acc
    | h::t -> f h (fold_right_helper acc t)
  in
  fold_right_helper init (List.rev lst)



(*función incseg que incrementa cada elemento de la lista por su posición *)
let incseg l =
  let len = List.length l in
  let rec incseg_helper idx acc =
    if idx = len then
      List.rev acc
    else
      let new_elem = List.nth l idx + idx in
      incseg_helper (idx + 1) (new_elem :: acc)
  in
  incseg_helper 0 []

