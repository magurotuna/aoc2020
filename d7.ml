open Base
open Stdio

let sample_file = "./inputs/d7_p1_sample.txt"

let input_file = "./inputs/d7_p1.txt"

let is_bag (s : string) : bool =
  let l =
    [ "bag"; "bags"; "bag,"; "bags,"; "bag."; "bags." ]
    |> List.filter ~f:(fun a -> String.equal a s)
    |> List.length
  in
  l > 0

let parse_color (words : string list) : string * string list =
  let rec inner acc rest =
    match rest with
    | [] -> assert false
    | first' :: rest' ->
        if is_bag first' then (acc, rest')
        else
          let next_s =
            if String.is_empty acc then first' else acc ^ " " ^ first'
          in
          inner next_s rest'
  in
  inner "" words

let consume_contain (words : string list) : string list =
  match words with
  | [] -> []
  | first :: rest -> if String.equal first "contain" then rest else words

let maybe_number (s : string) : int option =
  try Some (Int.of_string s) with Failure _ -> None

let parse_number (words : string list) : int * string list =
  match words with
  | [] -> (0, [])
  | first :: rest -> (
      match maybe_number first with None -> (0, words) | Some n -> (n, rest) )

let process_oneline acc cur =
  let words = String.split cur ~on:' ' in
  let color, rest = parse_color words in
  let rest = consume_contain rest in
  let rec loop acc' rest' =
    let num, rest = parse_number rest' in
    if num = 0 then acc'
    else
      let c, rest = parse_color rest in
      let next_map = Map.add_exn acc' ~key:c ~data:num in
      loop next_map rest
  in
  let emp = Map.empty (module String) in
  let data = loop emp rest in
  Map.add_exn acc ~key:color ~data

let rec can_contain_shiny_gold map color =
  Map.find_exn map color |> Map.keys
  |> List.exists ~f:(fun c ->
         String.equal c "shiny gold" || can_contain_shiny_gold map c)

let part1 () =
  let input = In_channel.read_lines input_file in
  let map =
    List.fold_left input ~init:(Map.empty (module String)) ~f:process_oneline
  in
  let ans =
    Map.keys map
    |> List.filter ~f:(fun c -> can_contain_shiny_gold map c)
    |> List.length
  in
  printf "ans: %d\n" ans

let solve_2 map =
  let rec inner map color =
    let target_color = Map.find_exn map color in
    Map.to_alist target_color
    |> List.map ~f:(fun (c, n) -> n + (n * inner map c))
    |> List.fold ~init:0 ~f:( + )
  in
  inner map "shiny gold"

let part2 () =
  let input = In_channel.read_lines input_file in
  let map =
    List.fold_left input ~init:(Map.empty (module String)) ~f:process_oneline
  in
  let ans = solve_2 map in
  printf "part2 ans: %d\n" ans
