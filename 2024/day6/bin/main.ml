open Base

let run state = 
  Sequence.unfold ~init:state ~f:(fun prev -> 
    match State.step prev with
    | Some nxt -> Some (nxt, nxt)
    | None -> None
    ) 
    |> Sequence.(append (singleton state))


let has_duplicates seq =
  let seen = Hash_set.create (module String) in
  Sequence.exists seq ~f:(fun id ->
    let has_seen = Hash_set.exists seen ~f:(String.equal id) in
    if has_seen then true else ( Hash_set.add seen id; false )
  )

let () =
  let initial_state = InputReader.read_lines ~path:"input" ~f:(String.to_list) |> State.of_input in
  let path = run initial_state |> Sequence.to_list in

  let positions = Set.of_list (module String) (List.map path ~f:(fun state -> 
    let (x, y) = State.pos state in 
    (Int.to_string x) ^ "," ^ (Int.to_string y))) in

  Set.length positions |> Stdio.printf "Part 1: %i\n";

  let test_states = List.map (Set.to_list positions) ~f:(fun pos -> 
    match String.split pos ~on:',' with
    | [x;y] -> State.add_obstacle initial_state (Int.of_string x, Int.of_string y)
    | _ -> failwith ""
  ) in

  let lst = List.map test_states ~f:(fun s -> run s |> Sequence.map ~f:State.id) in
  List.count lst ~f:(has_duplicates) |> Stdio.printf "Part 2: %i\n"
  (*Very slow but works*)
