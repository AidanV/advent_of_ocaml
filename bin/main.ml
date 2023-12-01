(*
get each line
filter only numbers
convert to int
sum list
*)

let file = "input.dat"

let num = Str.regexp {|[0-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}

let num_hash = Hashtbl.create 20

let _ = Hashtbl.add num_hash "0" 0
let _ = Hashtbl.add num_hash "1" 1
let _ = Hashtbl.add num_hash "2" 2
let _ = Hashtbl.add num_hash "3" 3
let _ = Hashtbl.add num_hash "4" 4
let _ = Hashtbl.add num_hash "5" 5
let _ = Hashtbl.add num_hash "6" 6
let _ = Hashtbl.add num_hash "7" 7
let _ = Hashtbl.add num_hash "8" 8
let _ = Hashtbl.add num_hash "9" 9
let _ = Hashtbl.add num_hash "zero" 0
let _ = Hashtbl.add num_hash "one" 1
let _ = Hashtbl.add num_hash "two" 2
let _ = Hashtbl.add num_hash "three" 3
let _ = Hashtbl.add num_hash "four" 4
let _ = Hashtbl.add num_hash "five" 5
let _ = Hashtbl.add num_hash "six" 6
let _ = Hashtbl.add num_hash "seven" 7
let _ = Hashtbl.add num_hash "eight" 8
let _ = Hashtbl.add num_hash "nine" 9

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(* let is_digit c = 
   '1' <= c && c <= '9' *)

(* let put i = 
   print_endline (string_of_int i);
   i
*)
let get_first_int s = 
  try 
    let _ = Str.search_forward num s 0 in
    Hashtbl.find num_hash (Str.matched_string s)
  with _ -> 0

let get_last_int s = 
  try
    let _ = Str.search_backward num s (String.length s) in
    Hashtbl.find num_hash (Str.matched_string s)
  with _ -> 0

let get_int s = 
  get_first_int s * 10 + get_last_int s

let () = read_lines file |> 
  List.filter (fun a -> a <> "") |>
  List.map get_int |>
  List.fold_left (+) 0 |>
  string_of_int |>
  print_endline


