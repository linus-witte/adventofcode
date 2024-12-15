open Base

(*------------------------- util -------------------------*)

let ( ++ ) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
let ( === ) (a1, b1) (a2, b2) = a1 = a2 && b1 = b2

let dir_of_char = function
  | '<' -> (-1, 0)
  | '>' -> (1, 0)
  | '^' -> (0, -1)
  | 'v' -> (0, 1)
  | _ -> failwith "unrecognized movement input"


let is map (x, y) c = Char.(map.(y).(x) = c)

let move_unsafe map (x, y) ch =
  let tx, ty = dir_of_char ch ++ (x, y) in
  map.(ty).(tx) <- map.(y).(x);
  map.(y).(x) <- '.';
  map


let move_wide_unsafe map (x, y) ch =
  let lst =
    match map.(y).(x) with
    | '@' -> [ (x, y) ]
    | '[' -> [ (x, y); (x + 1, y) ]
    | ']' -> [ (x - 1, y); (x, y) ]
    | _ -> failwith ""
  in

  let lst =
    if Char.(ch = '>') then
      List.rev lst
    else
      lst
  in

  List.iter lst ~f:(fun (x, y) ->
      let tx, ty = dir_of_char ch ++ (x, y) in
      map.(ty).(tx) <- map.(y).(x);
      map.(y).(x) <- '.');
  map


let find_all map ch =
  let height = Array.length map in
  let width = Array.length map.(0) in
  let positions = List.cartesian_product (List.init width ~f:Fn.id) (List.init height ~f:Fn.id) in
  List.filter positions ~f:(fun (x, y) -> is map (x, y) ch)


let find map ch = List.hd_exn (find_all map ch)

(*------------------------- part 1 -------------------------*)

let rec move map pos ch =
  if is map pos '#' then
    map
  else
    let target = dir_of_char ch ++ pos in
    if is map target '#' then
      map
    else if is map target 'O' then
      let map = move map target ch in
      if is map target '.' then
        move_unsafe map pos ch
      else
        map
    else if is map target '.' then
      move_unsafe map pos ch
    else
      map


let rec move_m map = function
  | [] -> map
  | hd :: tl ->
      let pos = find map '@' in
      move_m (move map pos hd) tl


let sum_crates map = find_all map 'O' |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + (100 * y) + x)

(*------------------------- part 2 -------------------------*)

let transform_map map =
  Array.map map ~f:(fun row ->
      Array.to_list row
      |> List.map ~f:(function
           | '#' -> [ '#'; '#' ]
           | 'O' -> [ '['; ']' ]
           | '.' -> [ '.'; '.' ]
           | '@' -> [ '@'; '.' ]
           | _ -> failwith "Unexpected character")
      |> List.concat |> Array.of_list)


(** Returns a list of position (positions contain #, \[ or \]) which have to move so that the crate or player at position [pos] can move in direrction of [ch].*)
let get_obstacles map (x, y) ch =
  let lst =
    match map.(y).(x) with
    | '@' -> [ (x, y) ]
    | '[' -> [ (x, y); (x + 1, y) ]
    | ']' -> [ (x - 1, y); (x, y) ]
    | _ -> failwith (Printf.sprintf "not recognized: %c    tried to move %c" map.(y).(x) ch)
  in

  let targets = List.map lst ~f:(( ++ ) (dir_of_char ch)) in
  List.filter targets ~f:(fun t -> not @@ (is map t '.' || List.mem lst t ~equal:(fun a b -> a === b)))


let rec can_move map pos ch =
  if is map pos '#' then
    false
  else
    let ob = get_obstacles map pos ch in
    List.length ob = 0 || List.for_all ob ~f:(fun p -> can_move map p ch)


let rec move_wide map pos ch =
  if is map pos '.' then
    map
  else if not @@ can_move map pos ch then
    map
  else
    let op = get_obstacles map pos ch in
    let map = List.fold op ~init:map ~f:(fun acc a -> move_wide acc a ch) in
    move_wide_unsafe map pos ch


let rec move_wide_m map = function
  | [] -> map
  | hd :: tl ->
      let pos = find map '@' in
      let next = move_wide_m (move_wide map pos hd) tl in
      next


let sum_crates_wide map = find_all map '[' |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + (100 * y) + x)

(*------------------------- entry point -------------------------*)

let () =
  let map =
    InputReader.read_lines_filter ~path:"input" ~f:(fun s ->
        if String.mem s '#' then
          Some (String.to_array s)
        else
          None)
    |> List.to_array
  in
  let movements =
    InputReader.read_lines_filter ~path:"input" ~f:(fun s ->
        if String.mem s '<' then
          Some (String.to_list s)
        else
          None)
    |> List.concat
  in

  let deep_copy arr = Array.map ~f:Array.copy arr in
  move_m (deep_copy map) movements |> sum_crates |> Stdio.printf "Part1: %i\n";
  transform_map map |> fun map -> move_wide_m map movements |> sum_crates_wide |> Stdio.printf "Part2: %i\n"
