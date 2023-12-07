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


(*let find_map_in_range (v: int) (map_ranges: map_range list) =
  let valid_ranges = List.filter (
    fun {map = _; range = (l, h)} ->
      (l <= v && v <= h)
  ) map_ranges in
  match valid_ranges with
  | x::_ -> x.map
  | [] -> (+) 0
;;*)

type bound = Lower of int * (int -> int) | Upper of int * (int -> int) | None;;

let get_next_map_range (map_ranges: map_range list) (v: int) =
  match List.find_opt (fun {map=_ ; range = (_, ub)} -> ub >= v) map_ranges with
  | Some {map = m; range = lb, ub} -> if lb > v then Lower (lb, m) else Upper (ub, m)
  | None -> None
;;

let rec get_mapped_ranges (map_ranges: map_range list) (input_ranges: (int * int) list) =
  match input_ranges with
  | [] -> []
  | (lb, ub)::rest -> 
    match get_next_map_range map_ranges lb with
    | Lower (nxt, _) ->
      if nxt > ub then (* was >*)
        (lb, ub)::(get_mapped_ranges map_ranges rest)
      else
        (lb, nxt-1)::(get_mapped_ranges map_ranges ((nxt, ub)::rest))
    | Upper (nxt, m) ->
      if nxt >= ub then (* was >*)
        (m lb, m ub)::(get_mapped_ranges map_ranges rest)
      else
        (m lb, m (nxt))::(get_mapped_ranges map_ranges ((nxt+1, ub)::rest))
    | None ->
      (lb, ub)::rest
;;

let get_paired_list ls =
  let left = List.filteri(fun i _ -> i mod 2 = 0) ls in
  let right = List.filteri(fun i _ -> (i + 1) mod 2 = 0) ls in
  List.combine left right
;;

let convert_to_upper_bound (start, range: int*int) =
  (start, start+range - 1)
;;

let () = 
  let lines = read_lines "input.dat" in
  let seeds = List.hd lines |> String.split_on_char ' ' |> List.tl |> List.map int_of_string |> get_paired_list in
  let lb_ub_seeds = List.map (convert_to_upper_bound) seeds |> List.sort(fun (l, _) (r, _) -> l - r) in
  let lines = List.tl lines |> List.tl in
  let splited = split_list_on_val lines "" in
  let splited = List.filter(fun a -> List.length a > 0) splited in
  let splited = List.map (List.tl) splited in
  let all_map_ranges = List.map (List.map make_map_range) splited |> List.map (List.sort (fun {map = _; range = (l, _)} {map = _; range = (r, _)} -> l - r)) in
  
  let final_ranges = 
    List.fold_left (fun lb_ub_s map_ranges ->
      get_mapped_ranges map_ranges lb_ub_s |> List.sort(fun (l, _) (r, _) -> l - r)
    ) lb_ub_seeds all_map_ranges in
  List.fold_left (fun prev (lb, _) -> if lb < prev then lb else prev) (Int.max_int) final_ranges |>

  string_of_int |>
  print_endline;
  
  
  
;;

