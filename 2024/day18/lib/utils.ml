open Base
module Hashtbl = Stdlib.Hashtbl

module PriorityQueue = struct
  type 'a t = ('a * float) list ref

  let create () = ref []
  let is_empty pq = List.is_empty !pq

  let push pq item priority =
    pq := List.sort ((item, priority) :: !pq) ~compare:(fun (_, p1) (_, p2) -> Float.compare p1 p2)


  let pop pq =
    match !pq with
    | [] -> failwith "is_empty"
    | (item, _) :: tl ->
        pq := tl;
        item
end

let ( ++ ) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
let ( -- ) (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)
let ( === ) (a1, b1) (a2, b2) = a1 = a2 && b1 = b2

let magnitude (a, b) =
  let open Float in
  (of_int a ** 2.0) +. (of_int b ** 2.0) |> sqrt


let in_bounds ~pos ~bounds =
  let x, y = pos in
  let w, h = bounds in
  x >= 0 && y >= 0 && x < w && y < h


let adj4 (x, y) =
  List.map ~f:(fun (off_x, off_y) -> (x + off_x, y + off_y)) [ (0, 1); (0, -1); (-1, 0); (1, 0) ]


let is map (x, y) c = Char.(map.(y).(x) = c)

let find_all map ch =
  let height = Array.length map in
  let width = Array.length map.(0) in
  let positions = List.cartesian_product (List.init width ~f:Fn.id) (List.init height ~f:Fn.id) in
  List.filter positions ~f:(fun (x, y) -> is map (x, y) ch)


let find map ch = List.hd_exn (find_all map ch)
let find_in_hashtable table key ~default = Hashtbl.find_opt table key |> Option.value ~default

let dijkstra ~(neighbors : 'a -> ('a * float) list) ~equal ~(start : 'a) =
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


let trace_back ~start predecessors =
  if not @@ Hashtbl.mem predecessors start then
    None
  else
    let rec aux current acc =
      if not @@ Hashtbl.mem predecessors current then
        Some (List.rev (current :: acc))
      else
        let pred = Hashtbl.find predecessors current in
        aux pred (current :: acc)
    in
    aux start []
