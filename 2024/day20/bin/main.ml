open MyBase
open Graph

module Node = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with
    | 0 -> Stdlib.compare y1 y2
    | c -> c


  let hash = Base.Hashtbl.hash
  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
end

module Int = struct
  type t = int

  let compare = compare
  let hash = Base.Hashtbl.hash
  let equal = ( = )
  let default = 0
end

module G = Imperative.Digraph.AbstractLabeled (Node) (Int)
open G

module W = struct
  type edge = G.E.t
  type t = int

  let weight x = G.E.label x
  let zero = 0
  let add = ( + )
  let sub = ( - )
  let compare = compare
end

module BF = Path.BellmanFord (G) (W)

let create_graph input =
  let height = Array.length input in
  let width = input.(0) |> Array.length in

  let g = G.create () in
  let vertex_tbl = Hashtbl.create () in
  let edge_tbl = Hashtbl.create () in

  Array.iteri input ~f:(fun y row ->
      Array.iteri row ~f:(fun x c ->
          if not (Char.equal c '#') then (
            let v = G.V.create (x, y) in
            G.add_vertex g v;
            Hashtbl.add_exn vertex_tbl ~key:(x, y) ~data:v)));

  G.iter_vertex
    (fun v1 ->
      let x, y = G.V.label v1 in
      List.map cardinals ~f:(fun (off_x, off_y) -> (x + off_x, y + off_y))
      |> List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
      |> List.filter ~f:(fun (x, y) -> not (Char.equal input.(y).(x) '#'))
      |> List.iter ~f:(fun (x, y) ->
          let v2 = Hashtbl.find_exn vertex_tbl (x, y) in
          match Hashtbl.find edge_tbl (v1, v2) with
          | Some e -> ignore ""
          | None -> begin
              let edge = G.E.create v1 1 v2 in
              G.add_edge_e g edge;
              Hashtbl.add_exn edge_tbl ~key:(v1, v2) ~data:edge
            end))
    g;

  let start = Hashtbl.find_exn vertex_tbl (CharGrid.find input 'S' |> Vector2.to_tuple) in
  let target = Hashtbl.find_exn vertex_tbl (CharGrid.find input 'E' |> Vector2.to_tuple) in
  (g, start, target)


let distance v1 v2 =
  let x1, y1 = G.V.label v1 in
  let x2, y2 = G.V.label v2 in
  abs (x1 - x2) + abs (y1 - y2)


(** Counts all cheats with positive savings *)
let count_cheats g sps spt no_cheat_cost max_cheat_length min_savings =
  let i = ref 0 in

  let stbl = Hashtbl.create () in

  G.iter_vertex
    (fun v1 ->
      G.iter_vertex
        (fun v2 ->
          let d = distance v1 v2 in
          if d >= 2 && d <= max_cheat_length then (
            let d1 = BF.H.find sps v1 in
            let d2 = BF.H.find spt v2 in
            let cost = d1 + d + d2 in
            let savings = no_cheat_cost - cost in
            if savings >= min_savings then
              i := !i + 1;
            match Hashtbl.find stbl savings with
            | Some i -> i := !i + 1
            | None ->
                let i = ref 1 in
                Hashtbl.add_exn stbl ~key:savings ~data:i))
        g)
    g;

  !i


let () =
  let input = read_lines ~path:"input" ~f:String.to_array |> List.to_array in
  let g, start, target = create_graph input in

  let sps = BF.all_shortest_paths g start in
  let spt = BF.all_shortest_paths g target in

  let no_cheat_cost = BF.H.find sps target in

  let part_a = count_cheats g sps spt no_cheat_cost 2 100 in
  let part_b = count_cheats g sps spt no_cheat_cost 20 100 in

  Stdio.printf "Number of cheats of length 2: %d\n" part_a;
  Stdio.printf "Number of cheats of length 20: %d" part_b
