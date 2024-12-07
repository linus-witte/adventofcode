open Base

let map_input line = 
  match String.split ~on:':' line with
  | [target; rest] -> (Int.of_string target, String.split ~on:' ' rest |> List.filter_map ~f:(Int.of_string_opt))
  | _ -> failwith "Invalid input file"


let results (t, lst) ~operators = 
  match lst with
  | hd :: rest -> 
      let res = List.fold rest ~init:[hd] ~f:(fun acc a -> 
        List.concat_map acc ~f:(fun x -> List.map operators ~f:(fun op -> op x a))) in
      (t, res)
  | _ -> failwith ""


let sum_valids lst = 
  List.filter_map lst ~f:(fun (t, values) -> 
    if List.find values ~f:(Int.equal t) |> Option.is_some then Some t else None)
  |> List.fold ~init:0 ~f:Int.(+)


let () =
  let input = InputReader.read_lines ~path:"input" ~f:map_input in

  let p1 = List.map input ~f:(results ~operators:[Int.(+); Int.( * )]) |> sum_valids in
  let p2 = List.map input ~f:(results ~operators:[Int.(+); Int.( * ); fun a b -> (Int.to_string a ^ Int.to_string b) |> Int.of_string]) |> sum_valids in

  Stdio.printf "Part 1: %i\n" p1;
  Stdio.printf "Part 2: %i\n" p2
