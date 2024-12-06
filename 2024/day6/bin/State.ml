open Base

type t = { 
  guard : (Int.t * Int.t);  
  guard_dir : (Int.t * Int.t); 
  map_size : (Int.t * Int.t);
  obstacles : (Int.t * Int.t) Sequence.t;
}

let find grid ch =
  List.findi grid ~f:(fun _ row -> List.exists row ~f:(fun c -> Char.equal c ch))
|> Option.bind ~f:(fun (row_idx, row) -> 
    let col_idx = List.findi row ~f:(fun _ c -> Char.equal c ch) |> Option.value_exn |> fst in
    Some (col_idx, row_idx)
    )

let filter_mapi2 grid ~f =
  List.concat_mapi grid ~f:(fun row_idx row -> 
    List.filter_mapi row ~f:(fun col_idx ch -> f col_idx row_idx ch)
    )

let (++) (a, b) (x, y) = (a + x, b + y)

let of_input input = 
  {
    guard = find input '^' |> Option.value_exn;
  guard_dir = (0, -1);
  map_size = (List.length input, List.length (List.nth_exn input 0));
  obstacles = filter_mapi2 input ~f:(fun x y ch -> if Char.equal ch '#' then Some (x,y) else None) |> Sequence.of_list
}

let rotate state = 
  {
    guard = state.guard;
  guard_dir = (function (0, 1) -> (-1, 0) | (-1, 0) -> (0, -1) | (0, -1) -> (1, 0) | (1, 0) -> (0, 1) | _ -> failwith "invalid rotation") state.guard_dir;
  map_size = state.map_size;
  obstacles = state.obstacles;
  }

let move state = 
  {
    guard = state.guard ++ state.guard_dir;
  guard_dir = state.guard_dir;
  map_size = state.map_size;
  obstacles = state.obstacles;
  }

let in_bounds bounds pos = 
  (fst pos) < (fst bounds) && (snd pos) < (snd bounds) && (fst pos >= 0) && (snd pos >= 0)

let step state = 
  let nxt_pos = state.guard_dir ++ state.guard in
  if not @@ in_bounds state.map_size nxt_pos then None
  else
    if Sequence.exists state.obstacles ~f:(fun (x, y) -> x = fst nxt_pos && y = snd nxt_pos) then
      Some (rotate state)
  else 
    Some (move state)


let pos state = (fst state.guard, snd state.guard)


let id state = 
  let pos = (fst state.guard |> Int.to_string) ^ "|" ^ (snd state.guard |> Int.to_string) in
  let rot = match state.guard_dir with | (0, 1) -> 0 | (-1, 0) -> 1 | (0, -1) -> 2 | (1, 0) -> 3 | _ -> -1 in
  pos ^ "|" ^ (Int.to_string rot)


let add_obstacle state pos = 
  {
    guard = state.guard;
    guard_dir = state.guard_dir;
    map_size = state.map_size;
    obstacles = Sequence.(append (singleton pos) state.obstacles);
  }

let to_string state = 
  let s = ref "" in
  for y = 0 to snd state.map_size do
  for x = 0 to fst state.map_size do
    let ch = 
      if (fst state.guard) = x && (snd state.guard) = y then '^' else
      if Sequence.exists state.obstacles ~f:(fun (ox, oy) -> ox = x && oy = y) then '#' else '.' in
      s := !s ^ Printf.sprintf "%c" ch
  done;
      s := !s ^ Printf.sprintf "\n%i\t"y
  done;
  !s
