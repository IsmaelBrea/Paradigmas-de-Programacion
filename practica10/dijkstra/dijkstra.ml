(* Archivo dijkstra.ml *)
open MinPrioQueue

exception Invalid_argument of string

let dijkstra graph =
  let n = Array.length graph in

  (* Verificar si la matriz es cuadrada *)
  if Array.exists (fun row -> Array.length row <> n) graph then
    raise (Invalid_argument "dijkstra: Matriz no cuadrada");

  (* Verificar que no haya valores negativos *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      match graph.(i).(j) with
      | Some weight when weight < 0 ->
          raise (Invalid_argument "dijkstra: Valores negativos no permitidos")
      | _ -> ()
    done
  done;

  (* Inicializar el grafo resultado *)
  let result = Array.make_matrix n n None in

  (* Crear una cola de prioridad *)
  let pq = MinPrioQueue.empty in

  (* Inicializar la cola de prioridad con el nodo inicial *)
  let pq =
    let pq_ref = ref pq in
    for i = 0 to n - 1 do
      match graph.(0).(i) with
      | Some weight ->
          pq_ref := MinPrioQueue.insert !pq_ref weight (0, i)
      | None -> ()
    done;
    !pq_ref
  in

  (* Algoritmo de Dijkstra *)
  let rec dijkstra_util pq =
    match MinPrioQueue.extract pq with
    | None -> result
    | Some (dist, (u, v), new_pq) ->
        if result.(u).(v) = None then begin
          result.(u).(v) <- Some dist;
          let updated_pq =
            let pq' = ref new_pq in
            for i = 0 to n - 1 do
              match graph.(v).(i) with
              | Some weight ->
                  pq' := MinPrioQueue.insert !pq' (dist + weight) (v, i)
              | None -> ()
            done;
            !pq'
          in
          dijkstra_util updated_pq
        end else dijkstra_util new_pq
  in

  dijkstra_util pq



