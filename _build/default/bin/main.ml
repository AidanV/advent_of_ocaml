let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let contains s1 s2 =
  let re = Str.regexp_string (" "^s2^" ") in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

(*let print_arr arr = 
  print_string "[ ";
  Array.iter (fun v -> 
  match v with
  | Some v -> print_string ((string_of_int v) ^ "; ")
  | None -> print_string "N/A; ") arr;
  print_endline "]"*)

let calculate_wins left right = 
  right
  |> String.split_on_char ' '
  |> List.filter ((<>) "")
  |> List.filter (fun s -> contains (left) s) 

let memoize = Array.make 300 (None);; 

let rec get_total all_wins n =
  match Array.get memoize n with
  | Some (x) -> x
  | None ->
    
  let num_wins = 
    match List.nth_opt all_wins n with
    | Some x -> List.length x
    | None -> 0
  in

  let vals = 
    List.init (num_wins) 
    (fun i -> get_total all_wins (i + n + 1)) 
  in

  let ret = List.fold_left (+) (1) vals in
  Array.set memoize n (Some ret);
  ret

let () = 
  let lines = read_lines "input.dat" |> List.filter ((<>) "") in 
  let lefts_and_rights = lines 
  |> List.map (String.split_on_char ':')
  |> List.map (fun l -> List.nth l 1)
  |> List.map (String.split_on_char '|')
  in
  let lefts = List.map (fun v -> List.nth v 0) lefts_and_rights in 
  let rights = List.map (fun v -> List.nth v 1) lefts_and_rights in
  let wins = 
    rights 
    |> List.mapi (fun card_num right -> calculate_wins (List.nth lefts card_num) right)
  in

  rights
  |> List.mapi (fun i _ -> get_total wins i)
  |> List.fold_left (+) (0)
  |> string_of_int
  |> print_endline

(*

lefts rights

*)
