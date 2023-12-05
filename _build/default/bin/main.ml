let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
;;

type map_range = {
  map: (int -> int);
  range: (int * int);
};;

let make_map_range s =
  let ls = String.split_on_char ' ' s in
  let ls = List.filter ((<>) "") ls in
  let first = int_of_string (List.nth ls 0) in
  let second = int_of_string (List.nth ls 1) in
  let third = int_of_string (List.nth ls 2) in
  {
    map = (+) (first - second); 
    range = (second, second + third - 1)
  }
;;

let rec index_of list v =
  match list with
  | [] -> None
  | x::_ when x = v -> Some 0
  | _::list -> 
    match index_of list v with
    | Some x -> Some (x + 1)
    | None -> None
;;

let rec split_list_on_val list (v: string) =
  match index_of list v with
  | Some i -> 
    (List.filteri (fun index _ -> i > index) list)::(split_list_on_val (List.filteri (fun index _ -> i < index) list) v)
  | None -> [list]
;;


let find_map_in_range (v: int) (map_ranges: map_range list) =
  let valid_ranges = List.filter (
    fun {map = _; range = (l, h)} ->
      (l <= v && v <= h)
  ) map_ranges in
  match valid_ranges with
  | x::_ -> x.map
  | [] -> (+) 0
;;

let () = 
  let lines = read_lines "input.dat" in
  let seeds = List.hd lines |> String.split_on_char ' ' |> List.tl |> List.map int_of_string in
  let lines = List.tl lines |> List.tl in
  let splited = split_list_on_val lines "" in
  let splited = List.filter(fun a -> List.length a > 0) splited in
  let splited = List.map (List.tl) splited in
  let map_ranges = List.map (List.map make_map_range) splited in
  let locations = List.map (
    fun seed ->
    List.fold_left (
      print_endline "";
      fun prev map_range -> 
      let map = find_map_in_range prev map_range in

      let total = map prev in
      print_endline ("(+) " ^ string_of_int (map 0) ^ "\t->\t" ^ string_of_int total);
      total
    ) seed map_ranges
    
  ) seeds in 
  List.fold_left (fun prev curr -> if curr < prev then curr else prev) (List.nth locations 0) locations |>
  string_of_int |>
  print_endline
  (*List.iter (fun s -> print_endline ""; List.iter (print_endline) s) splited;*)
  
  
  
;;
