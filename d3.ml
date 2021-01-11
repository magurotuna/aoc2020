open Base
open Stdio

let bump_tree str col =
  let len = String.length str in
  let col = col % len in
  let ch = str.[col] in
  let open Char in
  ch = '#'

let check_and_inc str col count = if bump_tree str col then count + 1 else count

let solve_part1 lst =
  let rec inner lst col count =
    match lst with
    | [] -> count
    | first :: rest ->
        let next_count = check_and_inc first col count in
        inner rest (col + 3) next_count
  in
  inner lst 0 0

let part1 () =
  let input = In_channel.read_lines "./inputs/d3_p1.txt" in
  let ans = solve_part1 input in
  printf "answer: %d\n" ans

let solve_part2_1 right lst =
  let rec inner lst col count =
    match lst with
    | [] -> count
    | first :: rest ->
        let next_count = check_and_inc first col count in
        inner rest (col + right) next_count
  in
  inner lst 0 0

let solve_part2_2 down lst =
  let rec inner lst col row count =
    match lst with
    | [] -> count
    | first :: rest ->
        if row % down = 0 then
          let next_count = check_and_inc first col count in
          inner rest (col + 1) (row + 1) next_count
        else inner rest (col + 1) (row + 1) count
  in
  inner lst 0 0 0

let part2 () =
  let input = In_channel.read_lines "./inputs/d3_p1.txt" in
  let pattern1 = solve_part2_1 1 input in
  let pattern2 = solve_part2_1 3 input in
  let pattern3 = solve_part2_1 5 input in
  let pattern4 = solve_part2_1 7 input in
  let pattern5 = solve_part2_2 2 input in
  let ans = pattern1 * pattern2 * pattern3 * pattern4 * pattern5 in
  printf "1: %d, 2: %d, 3: %d, 4: %d, 5: %d, ans: %d\n" pattern1 pattern2
    pattern3 pattern4 pattern5 ans
