open MyBase

type path = Vector2.t List.t [@@deriving compare, sexp_of]

let neighbors walls pos =
  adj4 pos
  |> List.filter ~f:(fun n -> not @@ List.mem walls n ~equal:Vector2.equal)
  |> List.map ~f:(fun p -> (p, 1.))


(*Find all paths from pos with length n*)
let n_paths ~bounds n pos =
  let rec aux n paths =
    if n <= 0 then
      paths
    else
      let new_paths =
        List.concat_map paths ~f:(fun path ->
            match path with
            | hd :: _ ->
                adj4 hd
                |> List.filter ~f:(fun pos ->
                       (not @@ List.mem path pos ~equal:Vector2.equal) && CharGrid.in_bounds ~bounds pos)
                |> List.map ~f:(fun p -> p :: path)
            | [] -> failwith "")
      in
      aux (n - 1) new_paths
  in
  let res = aux (n - 1) [ [ pos ] ] in
  res


let cheat_positions ~obstacles ~bounds path =
  let is_obstacle pos = List.mem obstacles pos ~equal:Vector2.equal in
  List.mapi path ~f:(fun i pos ->
      n_paths ~bounds 3 pos
      |> List.filter_map ~f:(fun path ->
             let hd = List.hd_exn path in
             if not @@ is_obstacle hd then
               Some (hd, i + 2)
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

  let predecessors, distances = dijkstra (module Vector2) ~neighbors:(neighbors walls) ~goal in

  let default_path = trace_back predecessors ~start |> Option.value_exn in
  let default_path_cost = Hashtbl.find_exn distances start in

  let positions = cheat_positions ~obstacles:walls ~bounds default_path in
  let res =
    List.count positions ~f:(fun (pos, cost) ->
        let cost_from_cheat = Hashtbl.find_exn distances pos in
        let new_cost = Float.of_int cost +. cost_from_cheat in
        Float.(default_path_cost - new_cost >= 100.))
  in
  Stdio.printf "Part 1: %i\n" res
