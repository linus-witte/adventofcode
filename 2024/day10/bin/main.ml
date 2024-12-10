open Base

let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
let ( ++ ) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
let in_bounds w h (a1, b1) = a1 >= 0 && b1 >= 0 && a1 < w && b1 < h

let step grid (x, y) =
  let n = grid.(y).(x) in
  List.(
    map dirs ~f:(( ++ ) (x, y))
    |> filter ~f:(in_bounds (Array.length grid.(0)) (Array.length grid))
    |> filter ~f:(fun (x, y) -> grid.(y).(x) = n - 1))


let step_path grid (path : (int * int) list) =
  step grid (List.hd_exn path) |> List.map ~f:(fun p -> p :: path)


let find_paths_downhill grid pos =
  let rec aux succs =
    match List.map succs ~f:(fun path -> step_path grid path) |> List.concat with
    | [] -> succs
    | lst -> aux lst
  in
  aux [ [ pos ] ]


let nines grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let nines = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if grid.(y).(x) = 9 then
        nines := (x, y) :: !nines
    done
  done;
  !nines


let () =
  let input =
    InputReader.read_lines ~path:"input" ~f:(fun lst ->
        lst |> String.to_array |> Array.map ~f:(fun lst -> String.of_char lst |> Int.of_string))
    |> List.to_array
  in

  let nines = nines input in

  let connections =
    List.map nines ~f:(find_paths_downhill input)
    |> List.concat
    |> List.filter ~f:(fun path -> List.length path = 10)
    |> List.map ~f:(fun path -> (List.nth_exn path 0, List.nth_exn path 9))
    |> List.dedup_and_sort ~compare:(fun ((sx1, sy1), (tx1, ty1)) ((sx2, sy2), (tx2, ty2)) ->
           match Int.compare sx1 sx2 with
           | 0 -> (
               match Int.compare sy1 sy2 with
               | 0 -> (
                   match Int.compare tx1 tx2 with
                   | 0 -> Int.compare ty1 ty2
                   | diff -> diff)
               | diff -> diff)
           | diff -> diff)
  in

  List.iter connections ~f:(fun ((sx, sy), (tx, ty)) -> Stdio.printf "(%i,%i) -> (%i, %i)\n" sx sy tx ty);
  Stdio.printf "Part 1: %i" (List.length connections)
