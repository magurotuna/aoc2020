open Base
open Stdio

let print_string_list lst = List.iter lst ~f:(fun s -> printf "%s\n" s)

let accumulate_until_blank_line lst =
  let rec inner lst acc =
    match lst with
    | [] -> (acc, [])
    | first :: rest ->
        if String.is_empty first then (acc, rest)
        else inner rest (acc ^ "\n" ^ first)
  in
  inner lst ""

let test_accumulate_until_blank_line =
  let t1 = [ "foo"; "bar"; ""; "maguro"; "tuna" ] in
  let acc, rest = accumulate_until_blank_line t1 in
  assert (String.equal acc "\nfoo\nbar");
  assert (List.equal String.equal rest [ "maguro"; "tuna" ])

let split_by_blank_line lst =
  let rec inner result input =
    match accumulate_until_blank_line input with
    | person, [] -> person :: result
    | person, rest ->
        let next_result = person :: result in
        inner next_result rest
  in
  inner [] lst

let split_by_whitespace_or_linebreak str =
  String.split str ~on:'\n'
  |> List.map ~f:(String.split ~on:' ')
  |> List.concat
  |> List.filter ~f:(fun s -> not (String.equal s ""))

let required_fields_ok field_list =
  let required_fields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] in
  List.for_all required_fields ~f:(fun req ->
      List.exists field_list ~f:(fun f -> String.equal f req))

let is_valid str =
  let split = split_by_whitespace_or_linebreak str in
  let fields = List.map split ~f:(String.sub ~pos:0 ~len:3) in
  let required_fields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] in
  List.for_all required_fields ~f:(fun req ->
      List.exists fields ~f:(fun f -> String.equal f req))

let solve_part1 lst = List.filter lst ~f:is_valid |> List.length

let test_split_by_blank_line =
  let t1 = [ "foo"; "bar"; ""; "maguro"; "tuna" ] in
  let result = split_by_blank_line t1 in
  assert (List.equal String.equal result [ "\nmaguro\ntuna"; "\nfoo\nbar" ])

let part1 () =
  let input = In_channel.read_lines "./inputs/d4_p1.txt" in
  let by_person = split_by_blank_line input in
  let ans = solve_part1 by_person in
  printf "%d\n" ans

let validate_byr value =
  try
    let num = Int.of_string value in
    1920 <= num && num <= 2002
  with Failure _ -> false

let validate_iyr value =
  try
    let num = Int.of_string value in
    2010 <= num && num <= 2020
  with Failure _ -> false

let validate_eyr value =
  try
    let num = Int.of_string value in
    2020 <= num && num <= 2030
  with Failure _ -> false

let validate_hgt value =
  let len = String.length value in
  if String.is_suffix value ~suffix:"in" then
    let unit_deleted = String.sub value ~pos:0 ~len:(len - 2) in
    try
      let num = Int.of_string unit_deleted in
      59 <= num && num <= 76
    with Failure _ -> false
  else if String.is_suffix value ~suffix:"cm" then
    let unit_deleted = String.sub value ~pos:0 ~len:(len - 2) in
    try
      let num = Int.of_string unit_deleted in
      150 <= num && num <= 193
    with Failure _ -> false
  else false

let test_validate_hgt =
  assert (validate_hgt "59in");
  assert (validate_hgt "76in");
  assert (validate_hgt "150cm");
  assert (validate_hgt "193cm")

let valid_char c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'a' | 'b' | 'c'
  | 'd' | 'e' | 'f' ->
      true
  | _ -> false

let validate_hcl value =
  if String.is_prefix value ~prefix:"#" then
    let len = String.length value in
    let pound_deleted = String.sub value ~pos:1 ~len:(len - 1) in
    if String.length pound_deleted = 6 then
      String.for_all pound_deleted ~f:valid_char
    else false
  else false

let validate_ecl value =
  match value with
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
  | _ -> false

let validate_pid value = String.length value = 9

let validate_value (key, value) =
  match key with
  | "byr" -> validate_byr value
  | "iyr" -> validate_iyr value
  | "eyr" -> validate_eyr value
  | "hgt" -> validate_hgt value
  | "hcl" -> validate_hcl value
  | "ecl" -> validate_ecl value
  | "pid" -> validate_pid value
  | "cid" -> true
  | _ -> assert false

let is_valid2 str =
  let split = split_by_whitespace_or_linebreak str in
  let field_value = List.map split ~f:(String.lsplit2_exn ~on:':') in
  let required_fields_satisfied =
    required_fields_ok (List.map field_value ~f:(fun (f, _) -> f))
  in
  let fields_validated = List.for_all field_value ~f:validate_value in
  required_fields_satisfied && fields_validated

let solve_part2 lst = List.filter lst ~f:is_valid2 |> List.length

let part2 () =
  let input = In_channel.read_lines "./inputs/d4_p1.txt" in
  let by_person = split_by_blank_line input in
  let ans = solve_part2 by_person in
  printf "%d\n" ans
