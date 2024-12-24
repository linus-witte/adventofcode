include Base
module Tuple2 = Batteries.Tuple2
module Tuple3 = Batteries.Tuple3
module Tuple4 = Batteries.Tuple4
module Tuple5 = Batteries.Tuple5

module Vector2 : sig
  type t = { x : int; y : int } [@@deriving compare, sexp_of]

  include Comparable.S with type t := t

  val of_tuple : int * int -> t
  val to_tuple : t -> int * int
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
end = struct
  module Inner = struct
    type t = { x : int; y : int } [@@deriving compare, sexp_of]

    let of_tuple (x, y) = { x; y }
    let to_tuple { x; y } = (x, y)
    let ( + ) a b = { x = a.x + b.x; y = a.y + b.y }
    let ( - ) a b = { x = a.x - b.x; y = a.y - b.y }
  end

  include Inner
  include Comparable.Make (Inner)
end

module Hashtbl = struct
  include Base.Hashtbl.Poly

  let find_or_default tbl key ~default = find tbl key |> Option.value ~default

  (** add or replace*)
  let replace tbl key value = Hashtbl.update tbl key ~f:(fun _ -> value)
end

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

let diags = [ (1, 1); (1, -1); (-1, -1); (-1, 1) ]
let cardinals = [ (0, 1); (0, -1); (-1, 0); (1, 0) ]

let adj4 pos =
  let open Vector2 in
  List.map ~f:(fun off -> of_tuple off + pos) cardinals


let adj8 pos =
  let open Vector2 in
  List.map ~f:(fun off -> of_tuple off + pos) (diags @ cardinals)


(********************* grid functions *********************)

module Grid (M : Comparable.S) = struct
  open Vector2

  type t = M.t array array

  let width grid = Array.length grid.(0)
  let height grid = Array.length grid

  let equal (g1 : t) (g2 : t) : bool =
    let open Int in
    if height g1 <> height g2 then
      false
    else
      Array.for_all2_exn g1 g2 ~f:(fun row1 row2 ->
          if Array.length row1 <> Array.length row2 then
            false
          else
            Array.for_all2_exn row1 row2 ~f:M.equal)


  let is grid pos value = M.equal grid.(pos.y).(pos.x) value

  let find_all grid value =
    let positions =
      List.cartesian_product (List.init (width grid) ~f:Fn.id) (List.init (height grid) ~f:Fn.id)
      |> List.map ~f:of_tuple
    in
    List.filter positions ~f:(fun pos -> is grid pos value)


  let find map value = List.hd_exn (find_all map value)

  let in_bounds ~bounds pos =
    let open Vector2 in
    let open Int in
    pos.x >= 0 && pos.y >= 0 && pos.x < bounds.x && pos.y < bounds.y
end

module CharGrid = Grid (Char)
module IntGrid = Grid (Int)

module Pathfinder = struct
  let dijkstra (type a) (module M : Comparable.S with type t = a) ~neighbors ~goal =
    let visited = Set.empty (module Vector2) |> Ref.create in
    let distances = Hashtbl.create () in
    let predecessor = Hashtbl.create () in
    let queue = PriorityQueue.create () in
    PriorityQueue.push queue goal 0.0;
    Hashtbl.add_exn distances ~key:goal ~data:0.;

    while not @@ PriorityQueue.is_empty queue do
      let current = PriorityQueue.pop queue in

      if not @@ Set.mem !visited current then
        visited := Set.add !visited current;

      List.iter (neighbors current) ~f:(fun (n, cost) ->
          let tentative_distance =
            Hashtbl.find_or_default distances current ~default:Float.infinity +. cost
          in
          if Float.( < ) tentative_distance (Hashtbl.find_or_default distances n ~default:Float.infinity) then (
            Hashtbl.replace distances n tentative_distance;
            Hashtbl.replace predecessor n current;
            PriorityQueue.push queue n tentative_distance))
    done;
    (predecessor, distances)


  let trace_back ~start predecessors =
    if not @@ Hashtbl.mem predecessors start then
      None
    else
      let rec aux current acc =
        if not @@ Hashtbl.mem predecessors current then
          Some (List.rev (current :: acc))
        else
          let pred = Hashtbl.find_exn predecessors current in
          aux pred (current :: acc)
      in
      aux start []
end

(** 
    Reads an input files and converts its to a list.
    Each list entry corresponds to line with function f applied to it.

    @param path The path to read from
    @param f The function to be applied to each list entry
    *)
let read_lines ~path ~f =
  let ic = Stdio.In_channel.create path in
  let rec next ic =
    match Stdio.In_channel.input_line ic with
    | Some line -> f line :: next ic
    | None ->
        Stdio.In_channel.close ic;
        []
  in
  next ic


let read_lines_filter ~path ~f = read_lines ~path ~f |> List.filter_opt
