(*
find all symbol positions -> hashmap?

find nums by splitting on all char not num

nums (list positions) value
*)

(* let find_numbers_with_symbols numbers symbols =*)


(* let convert_non_digit_to_space c = 
    match c with
    | '0'..'9' -> c
    | _ -> ' '*)

let is_symbol c =
  match c with
  | '0'..'9' | '.' -> false
  | _ -> true

let is_digit c = 
  match c with
  | '0'..'9' -> true
  | _ -> false

let is_symbol_around line_num symbols char_num = 
  List.fold_left (fun prev (l_num, c_num) -> (abs (line_num - l_num) <= 1 &&  abs (char_num - c_num) <= 1) || prev ) false symbols


let rec get_number is_around char_num chars = 
  match chars with
  | [] -> ("", false, [], char_num + 1)
  | x::chars when not (is_digit x) -> ("", false, chars, char_num + 1)
  | c::chars ->
    let (rest_num, around, ret_chars, curr_pos)  = get_number is_around (char_num+1) chars in
    (String.make 1 c ^ rest_num, is_around (char_num) || around, ret_chars, curr_pos)
(*returns option and rest of array*)
  
let rec get_all_numbers_from_line line_num line_chars curr_char_pos symbols =
  let is_around = is_symbol_around line_num symbols in
  let num = get_number (is_around) curr_char_pos line_chars in
  match num with
  | (num_string, around, [], _) ->
    if around 
    then [int_of_string num_string] 
    else []
  | (num_string, around, rest_chars, char_pos) ->
    if around 
    then (int_of_string num_string)::(get_all_numbers_from_line line_num rest_chars char_pos symbols) 
    else get_all_numbers_from_line line_num rest_chars char_pos symbols
(*
build up a number and only keep it if one of the values is next to a symbol
*)
  
  

let get_all_symbols_from_line line_num line = 
  String.to_seq line|>
  List.of_seq |>
  List.mapi((fun i c -> (c, i))) |>
  List.filter (fun (c, _) -> is_symbol c) |>
  List.map (fun (_, i) -> (line_num, i))
  

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents


let () = 
  let lines = read_lines "input.dat" |> List.filter ((<>) "") in 
  let symbols =  lines |> List.mapi (fun i s -> get_all_symbols_from_line i s) |> List.flatten in
  lines |> List.mapi (fun i line -> get_all_numbers_from_line i (List.of_seq (String.to_seq line)) 0 symbols) |> List.flatten 
  |> List.fold_left (+) 0
  |> string_of_int
  |> print_endline
  
  (* get_all_numbers_from_line
  (**) find_numbers_with_symbols 
  (lines |> List.mapi (fun i s -> get_all_numbers_from_line i (List.of_seq (String.to_seq s)) |> List.flatten) *)
