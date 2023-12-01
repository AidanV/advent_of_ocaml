(*
get each line
filter only numbers
convert to int
sum list
*)

let file = "input.dat"


let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let is_digit c = 
  '1' <= c && c <= '9' 

(* let put i = 
   print_endline (string_of_int i);
   i
*)
let get_int char_list = 
  ((List.find is_digit char_list |> int_of_char) - 48) * 10 + (List.find is_digit (List.rev char_list) |> int_of_char) - 48

let () = read_lines file |> 
  List.filter (fun a -> a <> "") |>
  List.map String.to_seq |> 
  List.map List.of_seq |> 
  List.map get_int|>
  List.fold_left (+) 0 |>
  string_of_int |>
  print_endline


