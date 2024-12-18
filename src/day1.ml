let read_file filename =
  let in_channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line in_channel in
      read_lines (line :: acc)
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  read_lines []

let lines = read_file "./inputs/01.txt"

let rec print_lines lines =
  match lines with
  | [] -> () (* TODO: Later make sure this is actually a unit (or void) *)
  | [ single ] -> print_endline single
  | head :: rest ->
      print_endline head;
      print_lines rest

let rows =
  lines
  |> List.map (fun row ->
         row |> String.split_on_char ' '
         |> List.filter_map (fun ch ->
                match ch with
                | "" -> None
                | strNum -> Some (int_of_string strNum)))

let print_int_list l = List.iter (Printf.printf "%d, ") l

let print_int_list_list il =
  List.iter
    (fun int_list ->
      print_int_list int_list;
      print_endline "")
    il

let get_columns input_matrix =
  let rec aux acc1 acc2 = function
    | [] -> (acc1, acc2)
    | [ int_list ] -> (List.nth int_list 0 :: acc1, List.nth int_list 1 :: acc2)
    | head :: rest ->
        aux (List.nth head 0 :: acc1) (List.nth head 1 :: acc2) rest
  in
  let col1, col2 = aux [] [] input_matrix in
  (List.rev col1, List.rev col2)

(* so I actually wanted (list1, list2), oh I'm so confused haha, I would just return a list of list but in column format maybe *)
(* oops, a blunder again, now I'm returning just a single list *)
(* I'm just freezing at times lol *)

let sum_list lst =
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> acc + x
    | x :: xs -> aux (acc + x) xs
  in
  aux 0 lst

let get_pairs_with_min_distance input_matrix =
  let first, second = get_columns input_matrix in
  let sortedFirst = List.sort compare first in
  let sortedSecond = List.sort compare second in
  List.combine sortedFirst sortedSecond
  |> List.map (fun (a, b) -> abs (a - b))
  |> sum_list
;;

(* looks like this wasn't a nice choice? *)
(* I wish I could just use indexes :p *)
(* okay, I'll take some AI help here *)
(* Oh wow there is a built-in for this *)

(* print_int_list column1;;
print_endline "";;
print_int_list column2 *)

Printf.printf "Result 1: %d" (get_pairs_with_min_distance rows)
