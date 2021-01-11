open Base
open Stdio

let rec print_list lst =
  match lst with
  | [] -> ()
  | first :: rest ->
      let _ = printf "%d\n" first in
      print_list rest

let exec () =
  let input_list = In_channel.read_lines "./inputs/d1_p1.txt" in
  let int_list = List.map input_list ~f:(fun s -> Int.of_string s) in
  let set = Set.of_list (module Int) int_list in
  let found =
    List.find int_list ~f:(fun n -> Set.exists set ~f:(fun e -> e + n = 2020))
  in
  match found with
  | None -> printf "Couldn't find the answer!\n"
  | Some x ->
      let ans = x * (2020 - x) in
      printf "x: %d, ans: %d\n" x ans

let rec sum_to_x_of_two_elements x lst =
  match lst with
  | [] -> None
  | first :: rest -> (
      match List.find rest ~f:(fun n -> n + first = x) with
      | None -> sum_to_x_of_two_elements x rest
      | Some y -> Some (first, y) )

let rec find_sum_2020_of_three lst =
  match lst with
  | [] -> None
  | first :: rest -> (
      match sum_to_x_of_two_elements (2020 - first) rest with
      | None -> find_sum_2020_of_three rest
      | Some (x, y) -> Some (first, x, y) )

let exec2 () =
  let int_list =
    In_channel.read_lines "./inputs/d1_p2.txt"
    |> List.map ~f:(fun s -> Int.of_string s)
  in
  match find_sum_2020_of_three int_list with
  | None -> printf "Couldn't find the answer\n"
  | Some (x, y, z) ->
      printf "(x, y, z) = (%d, %d, %d), answer = %d\n" x y z (x * y * z)
