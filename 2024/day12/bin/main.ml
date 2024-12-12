open Base

let ( ++ ) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
let adj4 pos = List.map [ (0, 1); (0, -1); (1, 0); (-1, 0) ] ~f:(( ++ ) pos)
let in_bounds w h (x, y) = x >= 0 && y >= 0 && x < w && y < h

type state = { width : int; height : int; seen : (int * int, char) Stdlib.Hashtbl.t; grid : char array array }

let region_of state x y =
  let region_type = !state.grid.(y).(x) in

  let rec dfs (x, y) : (int * int) list =
    if not (Stdlib.Hashtbl.mem !state.seen (x, y)) then (
      Stdlib.Hashtbl.add !state.seen (x, y) region_type;
      let succ =
        adj4 (x, y)
        |> List.filter ~f:(fun (nx, ny) -> in_bounds !state.width !state.height (nx, ny))
        |> List.filter ~f:(fun (nx, ny) -> Char.equal !state.grid.(ny).(nx) region_type)
        |> List.map ~f:dfs |> List.concat
      in
      (x, y) :: succ)
    else
      []
  in

  dfs (x, y)


let get_regions state =
  let regions = ref [] in

  for y = 0 to !state.height - 1 do
    for x = 0 to !state.width - 1 do
      if not (Stdlib.Hashtbl.mem !state.seen (x, y)) then
        let region = region_of state x y in
        regions := (!state.grid.(y).(x), region) :: !regions
    done
  done;
  !regions


let contains lst x = List.mem lst x ~equal:(fun (a1, b1) (a2, b2) -> a1 = a2 && b1 = b2)

(** Get directions of edges of [(x,y)]*)
let edges_of region (x, y) = List.filter (adj4 (0, 0)) ~f:(fun off -> contains region (off ++ (x, y)) |> not)

let perimiter region = List.fold region ~init:0 ~f:(fun acc pos -> acc + List.length (edges_of region pos))

let side_count region =
  let n = ref 0 in

  List.iter region ~f:(fun (x, y) ->
      let edges =
        edges_of region (x, y)
        |> List.filter ~f:(fun off ->
               match off with
               | 0, 1 -> not (contains region (x + 1, y) && contains (edges_of region (x + 1, y)) (0, 1))
               | 0, -1 -> not (contains region (x - 1, y) && contains (edges_of region (x - 1, y)) (0, -1))
               | 1, 0 -> not (contains region (x, y - 1) && contains (edges_of region (x, y - 1)) (1, 0))
               | -1, 0 -> not (contains region (x, y + 1) && contains (edges_of region (x, y + 1)) (-1, 0))
               | _ -> failwith "")
      in

      n := !n + List.length edges);
  !n


let () =
  let grid = InputReader.read_lines ~path:"input" ~f:String.to_array |> List.to_array in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let state = ref { width; height; seen = Stdlib.Hashtbl.create (width * height); grid } in

  let regions = get_regions state in
  List.fold regions ~init:0 ~f:(fun acc (_, region) -> acc + (List.length region * perimiter region))
  |> Stdio.printf "Part 1: %i\n";

  List.fold regions ~init:0 ~f:(fun acc (_, region) -> acc + (List.length region * side_count region))
  |> Stdio.printf "Part 2: %i\n"
