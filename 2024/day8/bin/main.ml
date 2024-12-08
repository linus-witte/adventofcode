open Base

module Vec = struct
  type t = int * int

  let of_tuple (x, y) : t = (x, y)
  let ( - ) ((a1, b1) : t) ((a2, b2) : t) : t = (a1 - a2, b1 - b2)
  let ( + ) ((a1, b1) : t) ((a2, b2) : t) : t = (a1 + a2, b1 + b2)
  let ( * ) x ((a, b) : t) : t = (x * a, x * b)
  let ( = ) ((a1, b1) : t) ((a2, b2) : t) = a1 = a2 && b1 = b2
  let ( <> ) v1 v2 = not (v1 = v2)
  let in_bounds w h ((x, y) : t) = x >= 0 && y >= 0 && x < w && y < h
  let to_string ((x, y) : t) = Printf.sprintf "Vec: (%i, %i)" x y

  let compare ((a1, b1) : t) ((a2, b2) : t) =
    match compare a1 a2 with
    | 0 -> compare b1 b2
    | c -> c
end

module PosSet = Stdlib.Set.Make (Vec)

let frequencies antennas =
  List.(
    map ~f:(fun (f, _, _) -> f) antennas
    |> dedup_and_sort ~compare:Char.compare
    |> map ~f:(fun f ->
           filter_map antennas ~f:(fun (c, x, y) ->
               if Char.equal f c then
                 Some (Vec.of_tuple (x, y))
               else
                 None)
           |> fun lst -> (f, PosSet.of_list lst)))


let pairs xs ys = List.(map xs ~f:(fun x -> map ys ~f:(fun y -> (x, y))) |> concat)

let antinodes set f =
  let lst = PosSet.to_list set in
  let pairs = pairs lst lst |> List.filter ~f:(fun (v1, v2) -> Vec.(v1 <> v2)) in

  let nodes (p1, p2) = f p1 p2 in
  List.map pairs ~f:nodes |> List.concat |> PosSet.of_list


let f2 p1 p2 =
  let open Vec in
  let d = p2 - p1 in
  Sequence.append
    (Sequence.unfold ~init:0 ~f:(fun s -> Some ((s * d) + p1, Int.(s + 1))) |> fun s -> Sequence.take s 100)
    (Sequence.unfold ~init:0 ~f:(fun s -> Some ((s * d) + p1, Int.(s - 1))) |> fun s -> Sequence.take s 100)
  |> Sequence.to_list


let () =
  let input = InputReader.read_lines ~path:"input" ~f:String.to_list in

  let height = List.length input in
  let width = List.(nth_exn input 0 |> length) in

  let antennas = ref [] in

  for y = 0 to height - 1 do
    let line = List.nth_exn input y in
    for x = 0 to width - 1 do
      let ch = List.nth_exn line x in
      if Char.(ch <> '.') then
        antennas := (ch, x, y) :: !antennas
    done
  done;

  let at = frequencies !antennas in
  let an lst f =
    List.unzip lst |> snd
    |> List.map ~f:(fun s -> PosSet.to_list (antinodes s f))
    |> List.concat |> PosSet.of_list
    |> PosSet.filter (Vec.in_bounds width height)
  in

  let f p1 p2 = [ Vec.(p2 + (p2 - p1)); Vec.(p1 + (p1 - p2)) ] in

  Stdio.printf "Part1: %i\n" (PosSet.to_list (an at f) |> List.length);
  Stdio.printf "Part2: %i\n" (PosSet.to_list (an at f2) |> List.length)
