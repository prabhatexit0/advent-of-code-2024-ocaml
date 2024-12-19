open Aocutils

let parse_input lines =
  let bucket =
    List.hd lines |> String.split_on_char ',' |> List.map String.trim
  in
  let patterns =
    List.filteri (fun i line -> i > 1 && String.length line > 0) lines
  in
  (bucket, patterns)

let rec ways_to_build_pattern bucket pattern cache =
  match Hashtbl.find_opt cache pattern with
  | Some count -> count
  | None ->
      if String.length pattern = 0 then 1
      else
        let count = ref 0 in
        List.iter
          (fun towel ->
            if String.starts_with ~prefix:towel pattern then
              let remaining_pattern =
                String.sub pattern (String.length towel)
                  (String.length pattern - String.length towel)
              in
              count :=
                !count + ways_to_build_pattern bucket remaining_pattern cache)
          bucket;
        Hashtbl.add cache pattern !count;
        !count

(* I will also solve this problem using tries *)

let part_one =
  let bucket, patterns = Aocutils.read_file "./inputs/19.txt" |> parse_input in
  let result =
    List.fold_left
      (fun acc pattern ->
        acc
        +
        if ways_to_build_pattern bucket pattern (Hashtbl.create 100) > 0 then 1
        else 0)
      0 patterns
  in
  Printf.printf "Result 1: %d\n" result

let part_two =
  let bucket, patterns = Aocutils.read_file "./inputs/19.txt" |> parse_input in
  let result =
    List.fold_left
      (fun acc pattern ->
        acc + ways_to_build_pattern bucket pattern (Hashtbl.create 100))
      0 patterns
  in
  Printf.printf "Result 2: %d\n" result
