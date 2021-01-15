open Base
open Stdio

let rec row (s : string) (from' : int) (end' : int) : int =
  let len = String.length s in
  if len = 0 then from'
  else
    let first_char = s.[0] in
    let rest = String.sub s ~pos:1 ~len:(len - 1) in
    if Char.equal first_char 'F' then row rest from' ((from' + end') / 2)
    else row rest (((from' + end') / 2) + 1) end'

let rec col (s : string) (from' : int) (end' : int) : int =
  let len = String.length s in
  if len = 0 then from'
  else
    let first_char = s.[0] in
    let rest = String.sub s ~pos:1 ~len:(len - 1) in
    if Char.equal first_char 'L' then col rest from' ((from' + end') / 2)
    else col rest (((from' + end') / 2) + 1) end'

let row_col (s : string) : int * int =
  let r = row (String.sub s ~pos:0 ~len:7) 0 127 in
  let c = col (String.sub s ~pos:7 ~len:3) 0 7 in
  (r, c)

let seat_id ((row, col) : int * int) : int = (row * 8) + col

let part1 () =
  let ans =
    In_channel.read_lines "./inputs/d5_p1.txt"
    |> List.map ~f:row_col |> List.map ~f:seat_id
    |> List.max_elt ~compare:(fun a b -> a - b)
  in
  match ans with
  | Some x -> printf "%d\n" x
  | None -> printf "Couldn't find the answer"

let part2 () =
  let exception Found of int in
  let ids =
    In_channel.read_lines "./inputs/d5_p1.txt"
    |> List.map ~f:row_col |> List.map ~f:seat_id
    |> Set.of_list (module Int)
  in
  let to' = seat_id (126, 7) in
  try
    for i = 8 to to' do
      let prev = i - 1 in
      let next = i + 1 in
      let cur_e = Set.mem ids i in
      let prev_e = Set.mem ids prev in
      let next_e = Set.mem ids next in
      if (not cur_e) && prev_e && next_e then raise (Found i) else ()
    done
  with Found x -> printf "%n\n" x
