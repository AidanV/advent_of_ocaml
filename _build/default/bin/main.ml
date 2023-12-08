let count_c_in_s (s: string) (c: char) =
  s
  |> String.to_seq
  |> Seq.fold_left (fun acc v -> if v = c then acc + 1 else acc) 0 
;;

type hand = Five of string | Four of string | House of string | Three of string | Two of string | One of string | High of string | Error;;

module Chars = Set.Make(Char)

let get_hand_type (s: string) =
  let char_set = Chars.of_seq (String.to_seq s) in
  let unique_chars = Chars.elements char_set in
  let length = List.length unique_chars in
  let num_J = count_c_in_s s 'J' in
  let has_J = num_J > 0 in
  match length with (* length is unique char - 1  AAJJJ  AAJJ9  AAAAA  AJA19    JJJJJ*)
  | 1 -> Five s
  | 2 -> (
      if has_J then Five s
      else (
        match count_c_in_s s (List.nth unique_chars 0) with
        | 1 | 4 -> Four s
        | 2 | 3 -> House s
        | _ -> Error
      )
    )
  | 3 -> 
    if count_c_in_s s (List.nth unique_chars 0) = 2 || 
       count_c_in_s s (List.nth unique_chars 1) = 2 
    then
      match num_J with
      | 0 -> Two s
      | 1 -> House s
      | 2 -> Four s
      | _ -> Error
    else
      if has_J then Four s else Three s
    
  | 4 -> if has_J then Three s else One s
  | 5 -> if has_J then One s else High s
  | _ -> Error
  (*
  if cset length 1 -> Five
  if cset length 2 {
    if c1 -> length 1 || 4 -> Four
    if c1 -> length 2 || 3 -> House
  }
  if cset length 3 {
    if c1 length 2 || c2 length 2 -> Two
    else -> Three
  }
  if cset length 4 -> two
  else high
  *)
;;


let compare_strings (s1: string) (s2: string) = 
  let card_hash = Hashtbl.create 13 in
  Hashtbl.add card_hash 'A' 12;
  Hashtbl.add card_hash 'K' 11;
  Hashtbl.add card_hash 'Q' 10;
  Hashtbl.add card_hash 'T' 9;
  Hashtbl.add card_hash '9' 8;
  Hashtbl.add card_hash '8' 7;
  Hashtbl.add card_hash '7' 6;
  Hashtbl.add card_hash '6' 5;
  Hashtbl.add card_hash '5' 4;
  Hashtbl.add card_hash '4' 3;
  Hashtbl.add card_hash '3' 2;
  Hashtbl.add card_hash '2' 1;
  Hashtbl.add card_hash 'J' 0;
  List.compare (fun a1 a2 -> Hashtbl.find card_hash a1 - Hashtbl.find card_hash a2) (List.of_seq (String.to_seq s1)) (List.of_seq (String.to_seq s2))
;;

(* 1 means hand1 is better, 0 is equal, -1 is hand2 is better*)
let compare_hands (hand1: hand) (hand2: hand) = 
  match hand1 with
  | Five s1 -> (
      match hand2 with
      | Five s2 -> compare_strings s1 s2
      | _ -> 1
    )
  | Four s1 -> (
      match hand2 with
      | Five _ -> -1
      | Four s2 -> compare_strings s1 s2
      | _ -> 1
    ) 
  | House s1 -> (
      match hand2 with
      | Five _ | Four _ -> -1
      | House s2 -> compare_strings s1 s2
      | _ -> 1
    )
  | Three s1 -> (
      match hand2 with
      | Two _ | One _ | High _ -> 1
      | Three s2 -> compare_strings s1 s2
      | _ -> -1
    )
  | Two s1 -> (
      match hand2 with
      | One _ | High _ -> 1
      | Two s2 -> compare_strings s1 s2
      | _ -> -1
    )
  | One s1 -> (
      match hand2 with
      | High _ -> 1
      | One s2 -> compare_strings s1 s2
      | _ -> -1
    )    
  | High s1 -> (
      match hand2 with
      | High s2 -> compare_strings s1 s2
      | _ -> -1
    )
  | Error -> 0 

;;



let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
;;

let () = 
  read_lines "input.dat"
  |> List.filter ((<>) "")
  |> List.map (String.split_on_char ' ') 
  |> List.map (fun ls -> (List.nth ls 0, int_of_string (List.nth ls 1))) 
  |> List.sort (fun (hand1, _) (hand2, _) -> compare_hands (get_hand_type hand1) (get_hand_type hand2))
  |> List.mapi (fun i (_, bid) -> bid * (i + 1))
  |> List.fold_left (fun acc curr -> acc + curr) 0
  |> print_int
;;    
  







(*
do a sort against the lines of input 

have the sort logic be where all of the comparison goes

*)
