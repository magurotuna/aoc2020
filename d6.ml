open Base
open Stdio

let count_uniq_chars (s : string) : int =
  s |> String.to_list |> Set.of_list (module Char) |> Set.length

let parse (input : string list) : string * string list =
  let rec loop acc input' =
    match input' with
    | [] -> (acc, [])
    | first :: rest ->
        if String.is_empty first then (acc, rest) else loop (acc ^ first) rest
  in
  loop "" input

let solve_1 (input : string list) : int =
  let rec loop count input' =
    let parsed, rest_list = parse input' in
    let uniq_count = count_uniq_chars parsed in
    let next_count = count + uniq_count in
    match rest_list with
    | [] -> next_count
    | otherwise -> loop next_count otherwise
  in
  loop 0 input

let sample_file = "./inputs/d6_p1_sample.txt"

let input_file = "./inputs/d6_p1.txt"

let part1 () =
  let input = In_channel.read_lines input_file in
  let ans = solve_1 input in
  printf "ans: %d\n" ans

let all_set =
  let a_cp = Char.to_int 'a' in
  let z_cp = Char.to_int 'z' in
  List.range a_cp (z_cp + 1)
  |> List.map ~f:Char.of_int_exn
  |> Set.of_list (module Char)

let parse_2 (input : string list) =
  let rec loop set input' =
    match input' with
    | [] -> (set, [])
    | first :: rest ->
        if String.is_empty first then (set, rest)
        else
          let cur_set = first |> String.to_list |> Set.of_list (module Char) in
          let next_set = Set.inter cur_set set in
          loop next_set rest
  in
  loop all_set input

let solve_2 (input : string list) : int =
  let rec loop count input' =
    let intersection, rest_list = parse_2 input' in
    let cur_count = Set.length intersection in
    let next_count = count + cur_count in
    match rest_list with
    | [] -> next_count
    | otherwise -> loop next_count otherwise
  in
  loop 0 input

let part2 () =
  let input = In_channel.read_lines input_file in
  let ans = solve_2 input in
  printf "ans: %d\n" ans
