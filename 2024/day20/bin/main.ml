open Base
open Util

type path = ((int * int) * int) list [@@deriving sexp_of]

let neighbors walls (x, y) =
  Utils.adj4 (x, y)
  |> List.filter ~f:(fun (n_x, n_y) -> not @@ List.mem walls (n_x, n_y) ~equal:Utils.( === ))
  |> List.map ~f:(fun p -> (p, 1.))


let cheat_positions ~obstacles ~bounds path =
  let width, height = bounds in
  let in_bounds_f (x, y) = x >= 0 && y >= 0 && x < width && y < height in
  List.mapi path ~f:(fun i pos ->
      Utils.adj4 pos
      |> List.map ~f:(fun p -> Utils.adj4 p |> List.map ~f:(fun n -> (p, n)))
      |> List.concat
      |> List.filter ~f:(fun (p1, p2) -> in_bounds_f p1 && in_bounds_f p2)
      |> List.filter ~f:(fun (_, p2) -> not @@ Utils.( === ) pos p2)
      |> List.filter ~f:(fun (p1, _) -> List.mem obstacles p1 ~equal:Utils.( === ))
      |> List.filter ~f:(fun (_, p2) -> not @@ List.mem obstacles p2 ~equal:Utils.( === ))
      |> List.map ~f:(fun (_, p2) -> (p2, i + 2)))
  |> List.concat


let () =
  let input = InputReader.read_lines ~path:"input" ~f:String.to_array |> List.to_array in
  let walls = Utils.find_all input '#' in
  let start = Utils.find input 'S' in
  let goal = Utils.find input 'E' in

  let width = Array.length input.(0) in
  let height = Array.length input in

  let predecessors = Utils.dijkstra ~neighbors:(neighbors walls) ~equal:Utils.( === ) ~start:goal in
  let default_path = Utils.trace_back predecessors ~start |> Option.value_exn in
  let default_path_cost = List.length default_path |> Float.of_int in

  let positions = cheat_positions ~obstacles:walls ~bounds:(width, height) default_path in
  let res =
    List.count positions ~f:(fun (pos, cost) ->
        let open Option.Monad_infix in
        let cost_from_cheat =
          if Utils.( === ) pos goal then
            1.
          else
            Utils.trace_back predecessors ~start:pos
            >>| List.length >>| Float.of_int |> Option.value ~default:Float.infinity
        in
        let new_cost = Float.of_int cost +. cost_from_cheat in
        Float.(default_path_cost - new_cost >= 100.))
  in

  Stdio.printf "Part 1: %i\n" res
