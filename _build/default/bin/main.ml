(*
A Y
B X
C Z

i could do hashmaps for each move
a -> Y: 6 X: 3 Z: 0
b -> ...

points -> Y: 2 X: 1 Z: 0

A rock
B paper
C scissors
X rock
Y paper
Z scissors
*)

let a = Hashtbl.create 3
let b = Hashtbl.create 3
let c = Hashtbl.create 3

let _ = Hashtbl.add a 'X' 3
let _ = Hashtbl.add a 'Y' 6
let _ = Hashtbl.add a 'Z' 0

let _ = Hashtbl.add b 'X' 0
let _ = Hashtbl.add b 'Y' 3
let _ = Hashtbl.add b 'Z' 6

let _ = Hashtbl.add c 'X' 6
let _ = Hashtbl.add c 'Y' 0
let _ = Hashtbl.add c 'Z' 3

let points = Hashtbl.create 3

let _ = Hashtbl.add points 'X' 1
let _ = Hashtbl.add points 'Y' 2
let _ = Hashtbl.add points 'Z' 3

let get_line_value s = 
  match s.[0] with
  | 'A' -> Hashtbl.find a s.[2] + Hashtbl.find points s.[2]
  | 'B' -> Hashtbl.find b s.[2] + Hashtbl.find points s.[2]  
  | 'C' -> Hashtbl.find c s.[2] + Hashtbl.find points s.[2]
  |  _ -> 0

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents


let () = 
  read_lines "input.dat" |>
  List.filter ((<>) "") |> 
  List.map (get_line_value) |>
  List.fold_left (+) 0|>
  string_of_int |>
  print_endline 
