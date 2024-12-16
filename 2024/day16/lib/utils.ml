open Base

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


let adj4 (x, y) =
  List.map ~f:(fun (off_x, off_y) -> (x + off_x, y + off_y)) [ (0, 1); (0, -1); (-1, 0); (1, 0) ]


let is map (x, y) c = Char.(map.(y).(x) = c)

let find_all map ch =
  let height = Array.length map in
  let width = Array.length map.(0) in
  let positions = List.cartesian_product (List.init width ~f:Fn.id) (List.init height ~f:Fn.id) in
  List.filter positions ~f:(fun (x, y) -> is map (x, y) ch)


let find map ch = List.hd_exn (find_all map ch)
