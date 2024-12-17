open Base
open Util

let trim s = Stdlib.String.trim s

type chip = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable ip : int;
  instructions : int array;
  mutable out_channel : int list;
}

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
      if not @@ equal chip.a zero then
        chip.ip <- operand
  (*bxc*)
  | 4 -> chip.b <- chip.b lxor chip.c
  (*out*)
  | 5 ->
      let out = combo_operand chip operand % 8 in
      chip.out_channel <- out :: chip.out_channel
  (*bdv*)
  | 6 ->
      let d = 2 ** combo_operand chip operand in
      chip.b <- chip.a / d
  (*cdv*)
  | 7 ->
      let d = 2 ** combo_operand chip operand in
      chip.c <- chip.a / d
  | _ -> failwith "unknown opcode"


let clone chip =
  {
    a = chip.a;
    b = chip.b;
    c = chip.c;
    ip = chip.ip;
    instructions = chip.instructions;
    out_channel = chip.out_channel;
  }


let initialize_chip () =
  let input =
    InputReader.read_lines_filter ~path:"input" ~f:(fun line ->
        match String.split line ~on:':' with
        | [ _; numbers ] ->
            String.split ~on:',' (trim numbers) |> List.map ~f:Int.of_string |> List.to_array |> Option.some
        | _ -> None)
    |> List.to_array
  in
  let instructions = input.(3) in
  { a = input.(0).(0); b = input.(1).(0); c = input.(2).(0); ip = 0; instructions; out_channel = [] }


let emulate chip =
  let copy = clone chip in
  let progam_length = Array.length copy.instructions in
  while copy.ip < progam_length do
    emulate_cycle copy
  done;
  copy.out_channel |> List.rev


let find_a chip =
  let instructions = chip.instructions |> Array.rev in
  let rec aux a prg_pos =
    if abs prg_pos >= Array.length instructions then
      Some a
    else
      let rec loop i =
        if i >= 8 then
          None
        else (
          chip.a <- (a * 8) + i;
          let hd = emulate chip |> List.hd_exn in
          if hd = instructions.(prg_pos) then
            match aux ((a * 8) + i) (prg_pos + 1) with
            | None -> loop (i + 1)
            | Some value -> Some value
          else
            loop (i + 1))
      in
      loop 0
  in
  aux 0 0


let rec print_output = function
  | [] -> ()
  | [ x ] -> Stdio.printf "%i\n" x
  | hd :: tl ->
      Stdio.printf "%i," hd;
      print_output tl


let () =
  let chip = initialize_chip () in
  Stdio.printf "Part 1:\t";
  emulate chip |> print_output;
  find_a chip |> Option.value_exn |> Stdio.printf "Part 2:\t%i\n"
