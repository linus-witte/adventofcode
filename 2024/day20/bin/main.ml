open MyBase

type path = Vector2.t List.t [@@deriving compare, sexp_of]

let neighbors walls pos =
  adj4 pos
  |> List.filter ~f:(fun n -> not @@ List.mem walls n ~equal:Vector2.equal)
  |> List.map ~f:(fun p -> (p, 1.))


let manhattan_positions ~distance ~in_bounds pos =
  let rec explore_positions pos acc remaining_distance =
    if remaining_distance = 0 then
      if in_bounds pos then
        pos :: acc
      else
        acc
    else
      adj4 pos |> List.fold ~init:acc ~f:(fun acc p -> explore_positions p acc (remaining_distance - 1))
  in
  explore_positions pos [] distance


let cheat_positions ~obstacles ~bounds ~cheat_len path =
  let is_obstacle pos = List.mem obstacles pos ~equal:Vector2.equal in
  List.mapi path ~f:(fun i pos ->
      manhattan_positions ~distance:(cheat_len + 1) ~in_bounds:(CharGrid.in_bounds ~bounds) pos
      |> List.filter_map ~f:(fun p ->
             if not @@ is_obstacle p then
               Some (p, i + 2)
             else
               None))
  |> List.concat


let () =
  let open CharGrid in
  let open Pathfinder in
  let open Vector2 in
  let input = read_lines ~path:"input" ~f:String.to_array |> List.to_array in
  let walls = find_all input '#' in
  let start = find input 'S' in
  let goal = find input 'E' in
  let bounds = { x = width input; y = height input } in

  let res = manhattan_positions ~distance:2 ~in_bounds:(CharGrid.in_bounds ~bounds) start in

  Stdio.printf "%s\n" (sexp_of_path res |> Sexp.to_string_hum)

(**)
(*let predecessors, distances = dijkstra (module Vector2) ~neighbors:(neighbors walls) ~goal in*)
(**)
(*let default_path = trace_back predecessors ~start |> Option.value_exn in*)
(*let default_path_cost = Hashtbl.find_exn distances start in*)
(**)
(*let positions = cheat_positions ~obstacles:walls ~bounds ~cheat_len:2 default_path in*)
(*let res =*)
(*  List.count positions ~f:(fun (pos, cost) ->*)
(*      let cost_from_cheat = Hashtbl.find_exn distances pos in*)
(*      let new_cost = Float.of_int cost +. cost_from_cheat in*)
(*      Float.(default_path_cost - new_cost >= 100.))*)
(*in*)
(*Stdio.printf "Part 1: %i\n%!" res;*)
(**)
(*let positions = cheat_positions ~obstacles:walls ~bounds ~cheat_len:20 default_path in*)
(*let res =*)
(*  List.count positions ~f:(fun (pos, cost) ->*)
(*      let cost_from_cheat = Hashtbl.find_exn distances pos in*)
(*      let new_cost = Float.of_int cost +. cost_from_cheat in*)
(*      Float.(default_path_cost - new_cost >= 100.))*)
(*in*)
(*Stdio.printf "Part 2: %i\n%!" res*)
