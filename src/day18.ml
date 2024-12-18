let read_file filename =
  let in_channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line in_channel in
      read_lines (line :: acc)
    with End_of_file ->
      close_in in_channel;
      List.rev acc (* Reverse the list to maintain the original order *)
  in
  read_lines []

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
  !level

let () =
  let n = 71 in
  let num_of_bytes = 1024 in
  let memory_space =
    read_file "./inputs/18.txt"
    |> parse_input
    |> List.filteri (fun i _ -> i < num_of_bytes)
    |> build_memory_space n
  in
  let result = find_min_steps_to_exit memory_space in
  Printf.printf "Result 1: %d\n" (result - 1)
