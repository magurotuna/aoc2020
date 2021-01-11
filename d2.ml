open Base
open Stdio

let get_range range =
  let left, right = String.lsplit2_exn range ~on:'-' in
  (Int.of_string left, Int.of_string right)

let get_ch ch =
  let open Char in
  let c = String.filter ch ~f:(fun c -> c <> ':') in
  of_string c

let is_ok (range, ch, password) =
  let left, right = get_range range in
  let ch = get_ch ch in
  let count =
    String.filter password ~f:(fun c ->
        let open Char in
        c = ch)
    |> String.length
  in
  if left <= count && count <= right then true else false

let process_line s =
  match String.split s ~on:' ' with
  | [ range; ch; password ] -> Some (range, ch, password)
  | _ -> None

let part1 () =
  let ans =
    In_channel.read_lines "./inputs/d2_p1.txt"
    |> List.filter_map ~f:process_line
    |> List.filter ~f:is_ok |> List.length
  in
  printf "%d\n" ans

let xor a b =
  let a_only = a && not b in
  let b_only = (not a) && b in
  a_only || b_only

let () =
  assert (xor true false);
  assert (xor false true);
  assert (not (xor true true));
  assert (not (xor false false))

let is_ok_part2_inner left right ch password =
  let left_ch = password.[left - 1] in
  let right_ch = password.[right - 1] in
  let open Char in
  let left_match = ch = left_ch in
  let right_match = ch = right_ch in
  xor left_match right_match

let is_ok_part2 (range, ch, password) =
  let left, right = get_range range in
  let ch = get_ch ch in
  is_ok_part2_inner left right ch password

let rec print_list lst =
  match lst with
  | [] -> ()
  | (range, ch, password) :: rest ->
      printf "range: %s, ch: %s, password: %s\n" range ch password;
      print_list rest

let part2 () =
  let satisfied_list =
    In_channel.read_lines "./inputs/d2_p1.txt"
    |> List.filter_map ~f:process_line
    |> List.filter ~f:is_ok_part2
  in
  let ans = List.length satisfied_list in
  printf "%d\n" ans

let () =
  assert (is_ok_part2_inner 6 7 'n' "jbncncnn");
  assert (is_ok_part2_inner 1 3 'a' "abced");
  assert (not (is_ok_part2_inner 1 3 'a' "aaaaa"));
  assert (not (is_ok_part2_inner 1 3 'a' "aaaaa"));
  printf "test success!\n"
