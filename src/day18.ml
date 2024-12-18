open Aocutils

let parse_input lines =
  List.map
    (fun line ->
      match String.split_on_char ',' line with
      | [ a; b ] -> (int_of_string b, int_of_string a)
      | _ -> failwith "Invalid input")
    lines

let build_memory_space n bytesList =
  let memory_space = Array.make_matrix n n '.' in
  List.iter (fun (a, b) -> memory_space.(a).(b) <- '#') bytesList;
  memory_space

let isValidLocation x y memory_space =
  x >= 0 && y >= 0
  && x < Array.length memory_space
  && y < Array.length memory_space.(0)
  && memory_space.(x).(y) != '#'

let find_min_steps_to_exit memory_space =
  let n = Array.length memory_space in
  let m = Array.length memory_space.(0) in
  let visited = Hashtbl.create 3500 in
  Hashtbl.add visited (0, 0) true;

  let directions = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
  let q = ref (Queue.create ()) in
  Queue.add (0, 0) !q;
  let found = ref false in
  let level = ref 0 in

  while (not (Queue.is_empty !q)) && not !found do
    let queue_size = ref (Queue.length !q) in

    for _ = 1 to !queue_size do
      let x, y = Queue.take !q in
      if (x, y) = (n - 1, m - 1) then found := true;

      List.iter
        (fun (dx, dy) ->
          let nx, ny = (x + dx, y + dy) in
          if
            isValidLocation nx ny memory_space
            && not (Hashtbl.mem visited (nx, ny))
          then (
            Hashtbl.add visited (nx, ny) true;
            Queue.add (nx, ny) !q))
        directions
    done;

    level := !level + 1
  done;
  (found, !level)

let rec will_prevent_exit n low high bytes =
  if low > high then -1
  else
    let mid = low + ((high - low) / 2) in
    let found, _ =
      List.filteri (fun i _ -> i <= mid) bytes
      |> build_memory_space n |> find_min_steps_to_exit
    in
    if not !found then
      let left_result = will_prevent_exit n low (mid - 1) bytes in
      if left_result != -1 then left_result else mid
    else will_prevent_exit n (mid + 1) high bytes

let part_one () =
  let n = 71 in
  let num_of_bytes = 1024 in
  let memory_space =
    Aocutils.read_file "./inputs/18.txt"
    |> parse_input
    |> List.filteri (fun i _ -> i < num_of_bytes)
    |> build_memory_space n
  in
  let _, result = find_min_steps_to_exit memory_space in
  Printf.printf "Result 1: %d\n" (result - 1)

let part_two () =
  let n = 71 in
  let bytes = Aocutils.read_file "./inputs/18.txt" |> parse_input in
  let index = will_prevent_exit n 0 (List.length bytes) bytes in
  let x, y = List.nth bytes index in
  Printf.printf "Result 2: %d,%d\n" y x

let () = part_two ()
