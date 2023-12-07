let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
;;

let find_roots (max_time: float) (distance: float) =
  ((Float.neg max_time +. Float.sqrt (Float.pow max_time 2. -. 4. *. distance)) /. -2., (Float.neg max_time -. Float.sqrt (Float.pow max_time 2. -. 4. *. distance)) /. -2.)
;;

let () = 

  let ls = read_lines "input.dat" 
  |> List.filter ((<>) "")
  |> List.map (String.split_on_char ':')
  |> List.map (fun ls -> List.nth ls 1)
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter ((<>) "")) 
  |> List.map (List.map(float_of_string)) in
  print_int (List.length ls);
  let pairs = List.combine (List.nth ls 0) (List.nth ls 1) in
    
  List.fold_left (fun acc (max_time, distance) ->
    let a, b = find_roots max_time distance in
    let a, b = 1 + int_of_float (Float.floor a), int_of_float (Float.ceil b) - 1 in
    let ret = (abs (a - b) + 1) in
    ret * acc
  ) 1 pairs
  |> string_of_int
  |> print_endline
  


;;
(*
(7-x) * x = 9

a = 1
b = max time
c = distance

-x^2 + bx - c

(-b +- sqrt(b^2 - 4c))/2


*)
