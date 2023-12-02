(*

split on ; 

regexp match [0-9] $color

nums are less than specified

fold &&

accumulate the valid lines

*)


let file = "input.dat"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let rec find_max_tuple tuples (ma, mb, mc) =
  match tuples with
  | [] -> (ma, mb, mc)
  | (a, b, c)::tuples -> find_max_tuple tuples (max a ma, max b mb, max c mc) 

let num_color s color = 
  let rgx = Str.regexp ({|\([0-9]+\) |}^color) in
  try
      let _ = Str.search_forward rgx s 0 in
      Str.matched_group 1 s |> int_of_string
  with _ -> 0

let product (a, b, c) = a * b * c 

let get_turn_tuple s = 
  (num_color s "red", num_color s "green", num_color s "blue")

let line_max s = 
  (String.split_on_char ';' s |>
  List.map get_turn_tuple |>
  find_max_tuple) (0, 0, 0)

let rec acc_valid_lines lines = 
  match lines with
  | [] -> 0
  | line::lines -> 
    product (line_max line) +  acc_valid_lines lines

let () = read_lines file |> List.filter ((<>) "") |> acc_valid_lines |> string_of_int |> print_endline
