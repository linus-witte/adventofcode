open Base
open Util

let start = (0, 0)
let goal = (70, 70)
let bounds = (71, 71)

let neighbors ~obstacles pos =
  Utils.adj4 pos
  |> List.filter ~f:(fun pos -> Utils.in_bounds ~pos ~bounds)
  |> List.filter ~f:(fun pos -> not @@ List.mem obstacles pos ~equal:Utils.( === ))
  |> List.map ~f:(fun pos -> (pos, 1.))


let () =
  let obstacles =
    InputReader.read_lines ~path:"input" ~f:(fun s ->
        match String.split s ~on:',' with
        | [ a; b ] -> (Int.of_string a, Int.of_string b)
        | _ -> failwith "")
  in

  let path =
    Utils.dijkstra
      ~neighbors:(neighbors ~obstacles:(List.take obstacles 1024))
      ~equal:Utils.( === ) ~start:goal
    |> Utils.trace_back ~start
  in

  Stdio.printf "Part1: %i\n%!" ((Option.value_exn path |> List.length) - 1);

  let start_search_at = 1025 in

  (*seq is a sequence of options of paths. The i'th element is the path from start to goal with with i obstacles. *)
  let seq =
    Sequence.unfold ~init:start_search_at ~f:(fun i ->
        let path =
          Utils.dijkstra
            ~neighbors:(neighbors ~obstacles:(List.take obstacles (i + 1)))
            ~equal:Utils.( === ) ~start:goal
          |> Utils.trace_back ~start
        in
        Some (path, i + 1))
  in

  (*This could be faster with binary search *)
  (*A-star and only recalculating the path if a new obstacle has fallen onto the path would help as well. *)
  let i =
    Sequence.findi seq ~f:(fun i path ->
        Stdio.printf "\rTesting %i out of %i%!" i (List.length obstacles - start_search_at);
        Option.is_none path)
    |> Option.value_exn |> fst |> Int.( + ) start_search_at
  in
  let x, y = List.nth_exn obstacles i in
  Stdio.printf "\nPart2: %i,%i\n" x y
