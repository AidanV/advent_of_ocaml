let file = "input.dat"


let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let rec get_first_elf ls = 
  match ls with
  | [] -> [], []
  | x::ns when x = "" -> [], ns
  | x::ns -> 
      let nums, rest = get_first_elf ns in
      (int_of_string x)::nums, rest

let rec get_elves ls = 
  match ls with
  | [] -> []
  | _ -> 
    let nums, rest = get_first_elf ls in
    nums::(get_elves rest)

let () = 
  read_lines file |> 
  get_elves |> 
  List.map (List.fold_left (+) 0) |> 
  List.fold_left max 0 |> 
  string_of_int |> 
  print_endline
  
