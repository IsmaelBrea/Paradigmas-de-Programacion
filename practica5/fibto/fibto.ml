(*Archivo fibto.ml *)


(*Función fib para calcular números de Fibonacci *)
let rec fib n =  
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

(* Función que genera una lista de números de Fibonacci hasta n *)
let rec fibonacci_until_n n i =
  if fib i > n then []
  else fib i :: fibonacci_until_n n (i + 1)

(* Función que imprime los números de Fibonacci menores o iguales a n *)
let print_fibonacci n =
  let fib_sequence = fibonacci_until_n n 0 in
  List.iter (fun x -> print_int x; print_newline ()) fib_sequence

(*Función principal *)
let () =
  if Array.length Sys.argv = 2 then
    let n = int_of_string Sys.argv.(1) in
    if n < 0 then
      Printf.eprintf "Error: El número debe ser no negativo.\n"
    else
      print_fibonacci n
  else
    Printf.eprintf "fibto: Número incorrecto de argumentos\n"

