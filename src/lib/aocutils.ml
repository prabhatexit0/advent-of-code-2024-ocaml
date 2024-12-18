module Aocutils = struct
  let print_character_matrix matrix =
    let rowCount = Array.length matrix in
    let colCount = Array.length matrix.(0) in
    for i = 0 to rowCount - 1 do
      for j = 0 to colCount - 1 do
        Printf.printf "%c " matrix.(i).(j)
      done;
      Printf.printf "\n"
    done

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
end
