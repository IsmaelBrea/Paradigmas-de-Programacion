(* Archivo list_loop.ml *)


let length lst =
  let count = ref 0 in
  let temp = ref lst in
  while !temp <> [] do
    count := !count + 1;
    temp := List.tl !temp
  done;
  !count
 
 
let last lst =
  let last_val = ref None in
  let temp = ref lst in
  while !temp <> [] do
    last_val := Some (List.hd !temp);
    temp := List.tl !temp
  done;
  match !last_val with
  | Some x -> x
  | None -> failwith "last"
  


let nth lst n =
  let index = ref 0 in
  let result = ref None in
  let temp = ref lst in
  while !temp <> [] do
    if !index = n then result := Some (List.hd !temp)
    else (
      temp := List.tl !temp;
      index := !index + 1
    );
  done;
  match !result with
  | Some x -> x
  | None -> failwith "nth"



let rev lst =
  let reversed = ref [] in
  let temp = ref lst in
  while !temp <> [] do
    reversed := List.hd !temp :: !reversed;
    temp := List.tl !temp
  done;
  !reversed


let append lst1 lst2 =
  let result = ref lst1 in
  let temp = ref lst2 in
  while !temp <> [] do
    result := !result @ [List.hd !temp];
    temp := List.tl !temp
  done;
  !result



let concat lst =
  let result = ref [] in
  let temp = ref lst in
  while !temp <> [] do
    result := !result @ (List.hd !temp);
    temp := List.tl !temp
  done;
  !result



let for_all predicate lst =
  let result = ref true in
  let temp = ref lst in
  while !temp <> [] && !result do
    result := predicate (List.hd !temp);
    temp := List.tl !temp
  done;
  !result



let exists predicate lst =
  let result = ref false in
  let temp = ref lst in
  while !temp <> [] && not !result do
    result := predicate (List.hd !temp);
    temp := List.tl !temp
  done;
  !result



let find_opt predicate lst =
  let result = ref None in
  let temp = ref lst in
  while !temp <> [] && !result = None do
    if predicate (List.hd !temp) then result := Some (List.hd !temp);
    temp := List.tl !temp
  done;
  !result



let iter f lst =
  let temp = ref lst in
  while !temp <> [] do
    f (List.hd !temp);
    temp := List.tl !temp
  done



let fold_left f acc lst =
  let result = ref acc in
  let temp = ref lst in
  while !temp <> [] do
    result := f !result (List.hd !temp);
    temp := List.tl !temp
  done;
  !result

