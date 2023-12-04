(*
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

make string out of left side

split right side into the individual values

filter right list if it is contained in the left string

map all cards to 2^(length-1)

sum

*)



let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let contains s1 s2 =
  let re = Str.regexp_string (" "^s2^" ") in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

let rec pow base exponent = 
  match exponent with
  | 0 -> 1
  | 1 -> base
  | _ -> base * pow base (exponent - 1)

let () = 
  let lines = read_lines "input.dat" |> List.filter ((<>) "") in 
  let lefts_and_rights = lines 
  |> List.map (String.split_on_char ':')
  |> List.map (fun l -> List.nth l 1)
  |> List.map (String.split_on_char '|')
  in
  let lefts = List.map (fun v -> List.nth v 0) lefts_and_rights in 
  let rights = List.map (fun v -> List.nth v 1) lefts_and_rights in
  rights 
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter ((<>) ""))
  |> List.mapi (fun card_num right -> List.filter (fun s -> contains (List.nth lefts card_num) s) right)
  |> List.map (List.length)
  |> List.filter (fun i -> i > 0)
  |> List.map (fun len -> pow 2 (len - 1))
  |> List.fold_left (+) 0
  |> string_of_int
  |> print_endline
