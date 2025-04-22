(*Archivo sort.ml *)

(*MÉTODO DE ORDENACIÓN POR INSERCIÓN: función isort*)
(*
let rec insert x = function
[] -> [x]
| h::t -> if x <= h then x :: h :: t
else h :: insert x t

let rec isort = function
[] -> []
| h::t -> insert h (isort t)
*)


(*definimos un valor bigl: int list tal que provoque un error de ejecución stack overflow al aplicarle isort*)
let bigl : int list =
  let rec generate_big_list acc n =
    if n <= 0 then acc
    else generate_big_list (n :: acc) (n - 1)
  in
  let result = generate_big_list [] 1000000 in
  result


(* Llamada que provocará el stack overflow al aplicarle isort *)
(* let result = isort bigl *)


(* funciones insert_t e isort_t de manera recursiva terminal*)
let insert_t x lst =
  let rec insert_tail acc = function
    | [] -> List.rev (x :: acc)
    | h :: t ->
        if x <= h then insert_tail (h :: acc) t
        else List.rev_append acc (x :: h :: t)
  in
  insert_tail [] lst

let rec isort_t lst =
  let rec isort_tail acc = function
    | [] -> List.rev acc
    | h :: t -> isort_tail (insert_t h acc) t
  in
  isort_tail [] lst


(* insert_t es recursiva terminal, por lo que para listas muy grandes seguirá funcionando, mientras que isort dará error de stack overflow*)
(* let result = isort_t bigl *)

  

(*definimos una "falsa" función rlist usando Random.int, que evite tener valores repetidos*)
let rlist n =
  let rec generate_unique acum = function
    | 0 -> acum
    | remaining ->
        let new_random = Random.int 1000 in
        if List.mem new_random acum then
          generate_unique acum remaining
        else
          generate_unique(new_random :: acum) (remaining - 1)
  in
  generate_unique [] n

  
  
(*definimos listas para comprobar cómo crece el tiempo de ejecución de isort e isort_t con la longitud de la lista*)
(*Lista creciente1 de 10.000 enteros*)
let lc1 =
  let rec aux acc count =
    if count <= 0 then acc
    else aux (count :: acc) (count - 1)
  in
  aux [] 10000

(*Lista creciente2 de 20.000 enteros*)
let lc2 =
  let rec aux acc count =
    if count <= 0 then acc
    else aux (count :: acc) (count - 1)
  in
  aux [] 20000


(*Lista decreciente1 de 10.000 enteros*)
let ld1 =
  let rec aux acc count =
    if count <= 0 then acc
    else aux (acc @ [count]) (count - 1)
  in
  aux [] 10000

let ld2 =
  let rec aux acc count =
    if count <= 0 then acc
    else aux (acc @ [count]) (count - 1)
  in
  aux [] 20000
  
  
(*Lista aleatoria1 de 10.000 enteros*)
let lr1 =
  let rec aux acc count =
    if count <= 0 then acc
    else aux (Random.int 1000 :: acc) (count - 1)
  in
  aux [] 10000


(*Lista aleatoria1 de 20.000 enteros*)
let lr2 =
  let rec aux acc count =
    if count <= 0 then acc
    else aux (Random.int 1000 :: acc) (count - 1)
  in
  aux [] 20000


(*¿como se comporta el tiempo de ejecución en los tres casos si duplicamos el tamaño de las listas? *)
(*Tanto las listas crecientes como las decrecientes como las aleatorias, al duplicar el tamaño de las listas, el tiempo de ejecución aumenta significativamente.Esto se puede deber a que el algoritmo de inserción, en el peor caso, tiene una complejidad cuadrática O(n^2). Al duplicar la lista, se incrementa la cantidad de comparaciones e inserciones, lo que lleva a un aumento exponencial en el tiempo de ejecución.*)


(*respecto a los tiempos de ejecución utiizando isort e isort_t en llas listas aleatorias lr1 y lr2, isort es más rápido en ambos casos.  A pesar de que isort_t está diseñado para ser recursivo terminal, su eficiencia se ve afectada por la reversión de listas y la gestión de acumuladores en comparación con isort, que tiene una estructura más simple y puede ser más eficiente en términos de tiempo de ejecución, especialmente cuando se trata de listas aleatorias *)


(*función recursiva terminal isort_g del algoritmo de ordenación por inserción y que tome como argumento la relación de orde que se desea emplear para la ordenación *)
let rec isort_g cmp lst =
  let rec insert_custom x acc =
    match acc with
    | [] -> [x]
    | h :: t ->
        if cmp x h then x :: h :: t
        else h :: insert_custom x t
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (insert_custom h acc) t
  in
  aux [] lst






(*MÉTODO DE ORDENACIÓN POR MEZCLA O FUSIÓN: función isort*)
(* 
let rec split l = match l with
h1::h2::t -> let t1, t2 = split t
in h1::t1, h2::t2
| _ -> l, []

let rec merge (l1,l2) = match l1, l2 with
[], l | l, [] -> l
| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, l2)
else h2 :: merge (l1, t2)

let rec msort l = match l with
[] | [_] -> l
| _ -> let l1, l2 = split l
in merge (msort l1, msort l2)
*)



(*msort produce el mismo resultado que isort:
let result_lc1 = msort lc1 = isort lc1
let result_lc2 = msort lc2 = isort lc2
let result_ld1 = msort ld1 = isort ld1
let result_ld2 = msort ld2 = isort ld2
let result_lr1 = msort lr1 = isort lr1
let result_lr2 = msort lr2 = isort lr2

val result_lc1 : bool = true
val result_lc2 : bool = true
val result_ld1 : bool = true
val result_ld2 : bool = true
val result_lr1 : bool = true
val result_lr2 : bool = true
*)


(*definimos un valor bigl2: int list tal que provoque un error de ejecución stack overflow al aplicarle msort*)
let bigl2 =
  let rec generate_big_list acc n =
    if n <= 0 then acc
    else generate_big_list (n :: acc) (n - 1)
  in
  let result = generate_big_list [] 1000000 in 
  result

(* Llamada que provocará el stack overflow al aplicarle msort *)
(* let result_bigl2 = msort bigl2 *) 



(*función split_t de manera recursiva terminal*)
let split_t lst =
  let rec split_accum acc1 acc2 = function
    | x :: y :: tl -> split_accum (x :: acc1) (y :: acc2) tl
    | x :: [] -> split_accum (x :: acc1) acc2 []
    | _ -> List.rev acc1, List.rev acc2
  in
  split_accum [] [] lst
  
  
  
(*función merge_t de manera recursiva terminal *)
let merge_t (lst1, lst2) =
  let rec merge_accum acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h2 :: t2 -> merge_accum (h2 :: acc) [] t2
    | h1 :: t1, [] -> merge_accum (h1 :: acc) t1 []
    | h1 :: t1, h2 :: t2 ->
        if h1 <= h2 then merge_accum (h1 :: acc) t1 (h2 :: t2)
        else merge_accum (h2 :: acc) (h1 :: t1) t2
  in
  merge_accum [] lst1 lst2


(* función msort' utilizando split_ty merge_t*)
let rec msort' lst =
  match lst with
  | [] | [_] -> lst
  | _ ->
      let l1, l2 = split_t lst in
      let sorted_l1 = msort' l1 in
      let sorted_l2 = msort' l2 in
      merge_t (sorted_l1, sorted_l2)


(* msort' es recursiva terminal, por lo que para listas muy grandes seguirá funcionando, mientras que msort dará error de stack overflow*)
(* let result_bigl2 = msort' bigl2 *)
let bigl3 : int list = []

(* definimos un valor bigl3, int list tal que provoque un error de ejecución stack overflow al aplicarle msort' *)
(*no es posible definir un valor big3 que produzca stack overflow al aplicarle msort', porque esa función no es recursiva terminal, y no necesariamente causará un desbordamiento de pila. Que no sea recursiva terminal signfica que aunque hay llamadas recursivas, hay trabajo adicional después de estas llamadas. La función merge_t se ejecuta después de las llamadas recursivas a msort'. Por lo tanto, al utilizar msort', no es posible generar un desbordamiento de pila (stack overflow) dado que no hay llamadas recursivas anidadas sin trabajo adicional pendiente *)


(*Para una lista de 10.000 y 20.000 elementos aleatorios, tanto msort como msort' tardan practicamente lo mismo. Ambos tiempos son menores que el isort. En general, para listas grandes, se espera que msort sea más rápido que isort. Esto es especialmente evidente cuando la lista es considerablemente grande debido a la diferencia en las complejidades de tiempo*)



(*función msort_g, que sea una versión de ordenación pot fusión que tome como argumento la relación de orden a emplear*)
let rec msort_g cmp lst =
  let rec merge acc l1 l2 =
    match l1, l2 with
    | [], _ -> List.rev_append acc l2
    | _, [] -> List.rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 then merge (h1 :: acc) t1 l2
        else merge (h2 :: acc) l1 t2
  in
  let rec split = function
    | [] | [_] as l -> l, []
    | h1 :: h2 :: tl ->
        let t1, t2 = split tl in
        h1 :: t1, h2 :: t2
  in
  match lst with
  | [] | [_] -> lst
  | _ ->
      let l1, l2 = split lst in
      let sorted_l1 = msort_g cmp l1 in
      let sorted_l2 = msort_g cmp l2 in
      merge [] sorted_l1 sorted_l2

  

