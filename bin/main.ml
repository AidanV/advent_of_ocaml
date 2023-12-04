let is_gear c = 
  match c with
  | '*' -> true
  | _ -> false

let is_digit c = 
  match c with
  | '0'..'9' -> true
  | _ -> false


let get_all_gears_from_line line_num line = 
  String.to_seq line|>
  List.of_seq |>
  List.mapi((fun i c -> (c, i))) |>
  List.filter (fun (c, _) -> is_gear c) |>
  List.map (fun (_, i) -> (line_num, i))
  

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents


(*includes original ch*)
let rec find_num_left line (ch:int) = 
  try
    match line.[ch] with
    | '0'..'9' -> find_num_left line (ch - 1) ^ String.make 1 line.[ch]
    | _ -> ""
  with _ -> ""

let rec find_num_right line (ch:int) = 
  try
    match line.[ch] with
    | '0'..'9' -> String.make 1 line.[ch] ^ find_num_right line (ch + 1)
    | _ -> ""
  with _ -> ""

let find_number line ch =
  
  try 
    let c = (line).[ch] in
    let left = find_num_left line ch in 
    let start = ch - String.length left + 1 in
    let right = find_num_right line (ch + 1) in
    if is_digit c then Some (start, left ^ right) else None
  with _ -> None

let is_not_already_in_list (a, b, c) list = 
  List.fold_left (fun prev (d, e, f) -> not (a = d && b = e && c = f) && prev) true list

let make_gear_set (lines: string list) gear = 
  let (gear_line, gear_ch) = gear in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line - 1)) (gear_ch - 1) with
    | Some(start, num_string) -> [(gear_line - 1, start, num_string)] 
    | None -> []
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line - 1)) (gear_ch) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line - 1, start, num_string) gear_numbers then (gear_line - 1, start, num_string)::gear_numbers else gear_numbers
    | None -> gear_numbers
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line - 1)) (gear_ch + 1) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line - 1, start, num_string) gear_numbers then (gear_line - 1, start, num_string)::gear_numbers else gear_numbers
    | None -> gear_numbers
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line)) (gear_ch - 1) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line, start, num_string) gear_numbers then (gear_line, start, num_string)::gear_numbers else  gear_numbers
    | None -> gear_numbers
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line)) (gear_ch + 1) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line, start, num_string) gear_numbers then (gear_line, start, num_string)::gear_numbers else gear_numbers
    | None -> gear_numbers
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line + 1)) (gear_ch - 1) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line + 1, start, num_string) gear_numbers then (gear_line + 1, start, num_string)::gear_numbers else gear_numbers
    | None -> gear_numbers
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line + 1)) (gear_ch) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line + 1, start, num_string) gear_numbers then (gear_line + 1, start, num_string)::gear_numbers else gear_numbers
    | None -> gear_numbers
  in
  let gear_numbers = 
    match find_number (List.nth lines (gear_line + 1)) (gear_ch + 1) with
    | Some(start, num_string) -> if is_not_already_in_list (gear_line + 1, start, num_string) gear_numbers then (gear_line + 1, start, num_string)::gear_numbers else gear_numbers
    | None -> gear_numbers
  in
  List.map (fun (_, _, num) -> int_of_string num) gear_numbers 
  

let () = 
  let lines = read_lines "input.dat" |> List.filter ((<>) "") in 
  let gears =  lines |> List.mapi (fun i s -> get_all_gears_from_line i s) |> List.flatten in
  List.map (fun gear -> make_gear_set lines gear) gears |>
  List.filter (fun ls -> List.length ls > 1) |>
  List.map (fun ls -> List.fold_left ( * ) 1 ls) |>
  List.fold_left (+) 0 |>
  string_of_int |>
  print_endline
