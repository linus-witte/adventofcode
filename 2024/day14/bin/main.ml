open Base

let width = 101
let height = 103

type robot = { mutable x : int; mutable y : int; vx : int; vy : int }

let rgx = Str.regexp {|p=\([0-9]+\),\([0-9]+\) v=\(-?[0-9]+\),\(-?[0-9]+\)|}

let parse_robot s =
  let a =
    if Str.string_match rgx s 0 then
      List.init 4 ~f:(fun i -> Str.matched_group (i + 1) s) |> List.map ~f:Int.of_string |> List.to_array
    else
      failwith (Printf.sprintf "Error in input data\n Could not parse: '%s'" s)
  in
  { x = a.(0); y = a.(1); vx = a.(2); vy = a.(3) }


let print_robots robots =
  let s = ref "" in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let n = List.count robots ~f:(fun robot -> robot.x = x && robot.y = y) in
      match n with
      | n when n > 0 -> s := !s ^ Int.to_string n
      | _ -> s := !s ^ "."
    done;
    s := !s ^ "\n"
  done;
  Stdio.printf "Grid:\n%s" !s


let tick robots =
  List.map robots ~f:(fun robot ->
      robot.x <- (robot.x + robot.vx) % width;
      robot.y <- (robot.y + robot.vy) % height;
      robot)


let rec tick_n robots = function
  | 0 -> robots
  | n -> tick_n (tick robots) (n - 1)


let safety_factor robots =
  let q1 = ref 0 in
  let q2 = ref 0 in
  let q3 = ref 0 in
  let q4 = ref 0 in

  List.iter robots ~f:(fun robot ->
      match robot with
      | r when r.x < width / 2 && r.y < height / 2 -> q1 := !q1 + 1
      | r when r.x > width / 2 && r.y < height / 2 -> q2 := !q2 + 1
      | r when r.x < width / 2 && r.y > height / 2 -> q3 := !q3 + 1
      | r when r.x > width / 2 && r.y > height / 2 -> q4 := !q4 + 1
      | _ -> ());

  !q1 * !q2 * !q3 * !q4


let neighbor_count robots robot =
  List.count robots ~f:(fun other -> Int.abs (other.x - robot.x) < 4 && Int.abs (other.y - robot.y) < 4)


let neighbor_coeff robots =
  let count = List.count robots ~f:(fun robot -> neighbor_count robots robot > 4) in
  Float.(of_int count / of_int (List.length robots))


let find_easter_egg robots =
  let rbs = ref robots in
  let i = ref 0 in
  let found = ref false in
  while not !found do
    let c = neighbor_coeff !rbs in
    if Float.(c > 0.65) then
      found := true
    else (
      rbs := tick !rbs;
      i := !i + 1)
  done;
  (!i, !rbs)


let () =
  let robots = InputReader.read_lines ~path:"input" ~f:parse_robot in

  tick_n robots 100 |> safety_factor |> Stdio.printf "Part 1: %i\n";
  Stdio.Out_channel.flush Stdio.stdout;

  let robots = InputReader.read_lines ~path:"input" ~f:parse_robot in
  let i, robots = find_easter_egg robots in
  print_robots robots;
  Stdio.printf "Part 2: %i\n" i
