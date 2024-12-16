open Base
open Util.Utils
module Hashtbl = Stdlib.Hashtbl

let find_in_hashtable table key ~default = Hashtbl.find_opt table key |> Option.value ~default

let dijstra ~(neighbors : 'a -> ('a * float) list) ~equal ~(start : 'a) =
  let visited = ref [] in
  let distances = Hashtbl.create 16 in
  let predecessor = Hashtbl.create 16 in
  let queue = PriorityQueue.create () in
  PriorityQueue.push queue start 0.0;
  Hashtbl.add distances start 0.;

  while not @@ PriorityQueue.is_empty queue do
    let current = PriorityQueue.pop queue in

    if not @@ List.mem !visited current ~equal then
      visited := current :: !visited;

    List.iter (neighbors current) ~f:(fun (n, cost) ->
        let open Float in
        let tentative_distance = find_in_hashtable distances current ~default:Float.infinity +. cost in
        if tentative_distance < find_in_hashtable distances n ~default:Float.infinity then (
          Hashtbl.replace distances n tentative_distance;
          Hashtbl.replace predecessor n current;
          PriorityQueue.push queue n tentative_distance))
  done;
  predecessor


let turn_right = function
  | 0, 1 -> (-1, 0)
  | -1, 0 -> (0, -1)
  | 0, -1 -> (1, 0)
  | 1, 0 -> (0, 1)
  | _ -> failwith ""


let turn_left dir = dir |> turn_right |> turn_right |> turn_right

let neighbors map (pos, dir) =
  if is map (pos ++ dir) '#' then
    [ ((pos, turn_left dir), 1001.); ((pos, turn_right dir), 1001.) ]
  else
    [ ((pos, turn_left dir), 1001.); ((pos, turn_right dir), 1001.); ((pos ++ dir, dir), 1.) ]


let rec trace_back predecessors start =
  if Hashtbl.mem predecessors start then
    start :: trace_back predecessors (Hashtbl.find predecessors start)
  else
    [ start ]


let path_cost path =
  let cost = ref 0.0 in
  let last = ref (List.hd_exn path |> snd) in
  List.iter path ~f:(fun (_, dir) ->
      cost :=
        !cost
        +.
        if dir === !last then
          1.0
        else
          1000.0;
      last := dir);
  Int.of_float !cost - 1


let () =
  let map = Util.InputReader.read_lines ~path:"input" ~f:String.to_array |> List.to_array in
  let predecessors =
    dijstra ~neighbors:(neighbors map)
      ~equal:(fun (a_pos, a_dir) (b_pos, b_dir) -> a_pos === b_pos && a_dir === b_dir)
      ~start:(find map 'E', (0, 1))
  in

  let best_path = trace_back predecessors (find map 'S', (1, 0)) in
  Stdio.printf "Part1: %i\n" (path_cost best_path)
