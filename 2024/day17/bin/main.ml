open Base
open Util

let trim s = Stdlib.String.trim s

type chip = { mutable a : int; mutable b : int; mutable c : int; mutable ip : int; instructions : int array }

let fetch_opcode chip =
  let opcode = chip.instructions.(chip.ip) in
  let operand = chip.instructions.(chip.ip + 1) in
  chip.ip <- chip.ip + 2;
  (opcode, operand)


let combo_operand chip = function
  | n when 0 <= n && n <= 3 -> n
  | 4 -> chip.a
  | 5 -> chip.b
  | 6 -> chip.c
  | n -> failwith (Printf.sprintf "invalid operand: %i" n)


let emulate_cycle chip =
  let opcode, operand = fetch_opcode chip in

  match opcode with
  (*adv*)
  | 0 ->
      let d = 2 ** combo_operand chip operand in
      chip.a <- chip.a / d
  (*bxl*)
  | 1 -> chip.b <- chip.b lxor operand
  (*bst*)
  | 2 -> chip.b <- combo_operand chip operand % 8
  (*jnz*)
  | 3 ->
      if not (chip.a = 0) then
        chip.ip <- operand
  (*bxc*)
  | 4 -> chip.b <- chip.b lxor chip.c
  (*out*)
  | 5 -> combo_operand chip operand % 8 |> Stdio.printf "%i,"
  (*bdv*)
  | 6 ->
      let d = 2 ** combo_operand chip operand in
      chip.b <- chip.a / d
  (*cdv*)
  | 7 ->
      let d = 2 ** combo_operand chip operand in
      chip.c <- chip.a / d
  | _ -> failwith "unknown opcode"


let initialize_chip () =
  let input =
    InputReader.read_lines_filter ~path:"input" ~f:(fun line ->
        match String.split line ~on:':' with
        | [ _; numbers ] ->
            String.split ~on:',' (trim numbers) |> List.map ~f:Int.of_string |> List.to_array |> Option.some
        | _ -> None)
    |> List.to_array
  in
  { a = input.(0).(0); b = input.(1).(0); c = input.(2).(0); ip = 0; instructions = input.(3) }


let () =
  let chip = initialize_chip () in

  let progam_length = Array.length chip.instructions in

  while chip.ip < progam_length do
    emulate_cycle chip
  done;
  Stdio.printf "\n"
