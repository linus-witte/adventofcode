open Base
module Arithmetic = Z3.Arithmetic
module Integer = Z3.Arithmetic.Integer
module Boolean = Z3.Boolean
module Symbol = Z3.Symbol
module Solver = Z3.Solver
module Model = Z3.Model

type machine = { ax : int; ay : int; bx : int; by : int; mutable px : int; mutable py : int }

let machine_regex =
  Str.regexp
    {|Button A: X\+\([0-9]+\), Y\+\([0-9]+\)Button B: X\+\([0-9]+\), Y\+\([0-9]+\)Prize: X=\([0-9]+\), Y=\([0-9]+\)|}


let parse_machine machine =
  let groups =
    if Str.string_match machine_regex machine 0 then
      Some
        (List.init 6 ~f:(fun i -> Str.matched_group (i + 1) machine)
        |> List.map ~f:Int.of_string |> List.to_array)
    else
      None
  in

  let res =
    match Option.value groups ~default:[||] with
    | [| ax; ay; bx; by; px; py |] -> Some { ax; ay; bx; by; px; py }
    | _ -> None
  in
  res


(*--------------------------------------------------------------------------*)

let get_model m =
  let ctx = Z3.mk_context [] in
  let a = Integer.mk_const ctx (Symbol.mk_string ctx "A") in
  let b = Integer.mk_const ctx (Symbol.mk_string ctx "B") in

  let ax = Arithmetic.mk_mul ctx [ a; Integer.mk_numeral_i ctx m.ax ] in
  let bx = Arithmetic.mk_mul ctx [ b; Integer.mk_numeral_i ctx m.bx ] in
  let x = Arithmetic.mk_add ctx [ ax; bx ] in
  let ay = Arithmetic.mk_mul ctx [ a; Integer.mk_numeral_i ctx m.ay ] in
  let by = Arithmetic.mk_mul ctx [ b; Integer.mk_numeral_i ctx m.by ] in
  let y = Arithmetic.mk_add ctx [ ay; by ] in

  let equal =
    Boolean.mk_and ctx
      [
        Boolean.mk_eq ctx x (Integer.mk_numeral_i ctx m.px);
        Boolean.mk_eq ctx y (Integer.mk_numeral_i ctx m.py);
      ]
  in

  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [ equal ];

  let _ = Solver.check solver [] in
  Solver.get_model solver
  |> Option.map ~f:(fun model ->
         ( Model.eval model a true |> Option.value_exn |> Integer.get_big_int,
           Model.eval model b true |> Option.value_exn |> Integer.get_big_int ))


let () =
  let input =
    InputReader.read_lines_filter ~path:"input" ~f:(function
      | "" -> None
      | s -> Some s)
  in
  let machines =
    Sequence.unfold ~init:input ~f:(fun s ->
        match s with
        | [] -> None
        | lst -> Some (List.take lst 3 |> String.concat |> parse_machine, List.drop lst 3))
    |> Sequence.filter_opt
  in

  let open Z in
  Sequence.fold machines ~init:zero ~f:(fun acc m ->
      match get_model m with
      | Some (a, b) -> acc + (Z.of_int 3 * a) + b
      | None -> acc)
  |> Z.to_string |> Stdio.printf "Part1: %s\n";

  Sequence.fold machines ~init:zero ~f:(fun acc m ->
      m.px <- Int.(m.px + 10000000000000);
      m.py <- Int.(m.py + 10000000000000);
      match get_model m with
      | Some (a, b) -> acc + (Z.of_int 3 * a) + b
      | None -> acc)
  |> Z.to_string |> Stdio.printf "Part2: %s\n"
