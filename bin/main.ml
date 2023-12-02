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


let num_color s color = 
  let rgx = Str.regexp ({|\([0-9]+\) |}^color) in
  try
      let _ = Str.search_forward rgx s 0 in
      Str.matched_group 1 s |> int_of_string
  with _ -> 0


let is_turn_valid s = 
  num_color s "red" <= 12 && num_color s "green" <= 13 && num_color s "blue" <= 14

let is_line_valid s = 
  String.split_on_char ';' s |>
  List.map is_turn_valid |>
  List.fold_left (&&) true

let rec acc_valid_lines num lines = 
  match lines with
  | [] -> 0
  | line::lines -> 
    if is_line_valid line
    then num + (acc_valid_lines (num + 1) lines) 
    else acc_valid_lines (num + 1) lines

let () = read_lines file |> List.filter ((<>) "") |> acc_valid_lines 1 |> string_of_int |> print_endline
