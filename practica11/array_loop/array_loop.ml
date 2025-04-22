(* Archivo array_loop.ml *)

let append arr1 arr2 =
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  let result = Array.init (len1 + len2) (fun i ->
    if i < len1 then arr1.(i) else arr2.(i - len1)
  ) in
  result



let sub arr start len =
  let result = Array.make len arr.(0) in
  let index = ref start in
  let i = ref 0 in
  while !i < len do
    result.(!i) <- arr.(!index);
    incr index;
    incr i
  done;
  result




let copy arr =
  let len = Array.length arr in
  let result = Array.make len arr.(0) in
  let i = ref 0 in
  while !i < len do
    result.(!i) <- arr.(!i);
    incr i
  done;
  result




let fill arr start len value =
  let i = ref start in
  let end_index = start + len in
  while !i < end_index do
    arr.(!i) <- value;
    incr i
  done




let blit src src_pos dst dst_pos len =
  let i = ref 0 in
  while !i < len do
    dst.(dst_pos + !i) <- src.(src_pos + !i);
    incr i
  done




let to_list arr =
  let len = Array.length arr in
  let result = ref [] in
  let i = ref 0 in
  while !i < len do
    result := arr.(!i) :: !result;
    incr i
  done;
  List.rev !result




let iter f arr =
  let len = Array.length arr in
  let i = ref 0 in
  while !i < len do
    f arr.(!i);
    incr i
  done



let fold_left f acc arr =
  let result = ref acc in
  let len = Array.length arr in
  let i = ref 0 in
  while !i < len do
    result := f !result arr.(!i);
    incr i
  done;
  !result




let for_all predicate arr =
  let len = Array.length arr in
  let result = ref true in
  let i = ref 0 in
  while !i < len && !result do
    result := predicate arr.(!i);
    incr i
  done;
  !result

let exists predicate arr =
  let len = Array.length arr in
  let result = ref false in
  let i = ref 0 in
  while !i < len && not !result do
    result := predicate arr.(!i);
    incr i
  done;
  !result




let find_opt predicate arr =
  let len = Array.length arr in
  let result = ref None in
  let i = ref 0 in
  while !i < len && !result = None do
    if predicate arr.(!i) then result := Some arr.(!i);
    incr i
  done;
  !result

