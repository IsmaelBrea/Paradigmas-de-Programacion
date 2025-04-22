(*Archivo mylist.ml *)

(*29 Funciones recursivas de listas *)

(*1-función hd que devuelve la cabeza de la lista *)
let hd = function
	[] -> raise(Failure "hd")
        |h::_ -> h
     
(*2-función tl que devuelve la cola de la lista *)  
let tl = function
	[] -> raise(Failure "tl")
	|h::t -> t
   

(*3-función recursiva terminal length, devuelve la longitud de una lista*)
let rec length = function
    [] -> 0
    | _::t -> 1 + length t
 
 
 
(*4-función recursiva terminal  compare_lengths -> devuelve 0 sin tienen la misma longitud, 1 si la primera es mayor que la segunda y -1 si la segunda es mayor que la primera *)
let rec compare_lengths  lista1 lista2 =
    match lista1,lista2 with
          ([],[])-> 0
          |(_,[])-> 1
          |([],_)-> -1
          |(_::t1,_::t2)->
              compare_lengths t1 t2
            
            
            
(*5-función recursiva terminal compare_lengths_with -> compara la longitud de una lista con un entero*)
let compare_length_with lista len =
  let rec compare_length_helper lista len acc =
    match lista, len with
    | [], 0 -> acc
    | [], _ -> acc - len
    | _::t, n when n > 0 -> compare_length_helper t (n - 1) (acc + 1)
    | _ -> invalid_arg "Length negativa"
  in
  if len < 0 then
    invalid_arg "Length negativa"
  else
    compare_length_helper lista len 0
    


(*6-función recursiva terminal init, crea una lista que contiene valores generados a partir de una función dada *)
let rec init n f =
  let rec aux i acc =
    if i < 0 then
      failwith "init: El número de elementos debe ser no negativo"
    else if i = 0 then
      acc
    else
      aux (i - 1) (f i :: acc)
  in
  aux n []

  

(*7-función recursiva terminal  nth, develve el elemento de una posición especificada *)
let rec nth l n = if n < 0 then raise(Failure "Illegal argument") else match l, n with
	[], _ -> raise(Failure "nth")
	|h::_, 0 -> h
	|_::t, _ -> nth l (n-1)


(*8-función NO RECURSIVA TERMINAL append, que concatena listas *)
let rec append list1 list2 =
  match list1 with
  | [] -> list2
  | h :: t -> h :: append t list2


	
(*9-función recursiva terminal rev_append, revierte una lista y la concatena con otra lista *)
let rec rev_append l1 l2 = match l1 with
	[] -> l2
	|h::t -> rev_append t (h::l2)



(*10-función rev, devuelve la lista reinvertida *)
let rev lista =
   let rec aux lista a = match lista with
      [] -> a
      | h::t -> aux t (h::a)
   in aux lista []


(*11-función NO RECURSIVA TERMINAL concat, que concatena dos listas*)
let rec concat = function 
    [] -> []
    | h::t -> append h (concat t)
    
    
(*12-función NO RECURSIVA TERMINAL flatten, aplana una lista de listas. Es lo mismo que concat*)
let rec flatten = function 
    [] -> []
    | h::t -> append h (flatten t)
    

(*13-función NO RECURSIVA TERMINAL split, divide una lista en dos listas *)
let rec split = function 
    [] -> ([],[])
    | (h1,h2)::t -> let t1,t2 =
        split t in h1::t1,h2::t2


(*14-función NO RECURSIVA TERMINAL combine, combina dos listas en una lista de pares (tuplas) *)
let rec combine lista1 lista2 =
    match (lista1, lista2) with 
    [], [] -> []
    |h1::t1, h2::t2 -> (h1,h2) :: (combine (t1) (t2))
    |_ -> raise (Invalid_argument "combine")
    
    

(*15-función NO RECURSIVA TERMINAL map, aplica una función a cada elemento de la lista y devuelve una nueva lista con los resultados *)
let rec map f = function 
    [] -> []
    |h :: t -> (f h) :: (map f t)
    
    

(*16-función NO RECURSIVA TERMINAL map2, aplica una función a cada elemento de dos listas diferentes y devuelve una nueva lista con los resultados *)
let rec map2 f l1 l2 =
  match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (h1 :: t1, h2 :: t2) -> (f h1 h2) :: map2 f t1 t2



(*17-función recursiva terminal rev_map, aplica una función a cada elemento de la lista y acumula los resultados en orden inverso *)
let rec rev_map f l = 
    let rec aux l auxl = match l with 
        [] -> auxl
        | h :: t -> aux t (f(h):: auxl)
    in aux l []



(*18-función recursiva terminal for_all, verifica si una propiedad se cumple para todos los elementos de la lista*)
let rec for_all p = function 
    []-> true 
    |h::t -> (p h) && (for_all p t)


(*19-función recursia terminal exists, comprueba si algún elemento en una lista cumple cierta propiedad *)
let rec exists p = function 
    []-> false
    |h :: t -> (p h ) || (exists p t)



(*20-función recursiva terminal mem, verifica si un elemento está en la lista*)
let rec mem p = function
    [] -> false 
    |h::t -> if (p = h )then true 
             else (mem p t)



(*21-función recursiva terminal find, busca el primer elemento de una lista que cumple una condición dada*)
let rec find p l = match l with
	[] -> raise(Not_found)
	|h::t -> if p h then h else find p t



(*22-función recursiva terminal filter, busca los elementos de la lista que cumplen una condición y devuelve una lista con los elementos que cumplen la condición*)
let rec filter f l = match l with 
	[] -> []
	|h::t -> if f h then h::filter f t else filter f t


(*23-función recursiva terminal find_all, es lo mismo que filter *)
let rec find_all f l = match l with 
	[] -> []
	|h::t -> if f h then h::find_all f t else filter f t



(*24-función recursiva terminal partition, toma una lista y en función de una condició divide la lista en dos subconjuntos *)
let partition p l =
  let rec aux si no = function
    | [] -> (List.rev si, List.rev no)
    | h::t ->
      if p h then
        aux (h::si) no t
      else
        aux si (h::no) t
  in
  aux [] [] l


(*25-función recursiva terminal fold_left,aplica una función a cada elemento de una lista, acumulando un valor a medida que se recorre la lista de izquierda a derecha *)
let rec fold_left op e = function
	[] -> e
	|h::t -> fold_left op (op e h) t	


(*26-función NO RECURSIVA TERMINAL fold_rigth,aplica una función a cada elemento de una lista, acumulando un valor a medida que se recorre la lista de derecha a izquierda *)
let rec fold_right op l e = match l with
	[] -> e
	|h::t -> op h (fold_right op t e)


(*27-función recursiva terminal assoc,toma un valor a y una lista de pares (a, b) y devuelve el valor asociado a la clave a en la lista de pares l. Si (a, b) es el primer emparejamiento de izquierda a derecha en la lista l, entonces assoc a l devolverá b. Si no se encuentra un valor asociado a a en la lista l, la función lanzará una excepción *)
let rec assoc key = function
  | [] -> raise Not_found
  | (k, v)::t -> if k = key then v else assoc key t


(*28-función recursiva terminal mem_assoc, verifica si una clave específica está presente en una lista de pares clave-valor *)
let mem_assoc key lst =
  let rec aux acc = function
    | [] -> acc
    | (k, _) :: t when k = key -> true
    | _ :: t -> aux acc t
  in
  aux false lst



(*29-función NO RECURSIVA TERMINAL remove_assoc, elimina todas las ocurrencias de la clave key en la lista de pares clave-valor *)
let rec remove_assoc key lst =
  match lst with
  | [] -> []
  | (k, v) :: t when k = key -> remove_assoc key t
  | hd :: t -> hd :: remove_assoc key t













