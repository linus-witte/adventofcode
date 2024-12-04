open Base

let diags = [ (1, -1); (-1, 1); (1, 1); (-1,-1) ] 
let directions = diags @ [ (-1, 0); (0, -1); (0, 1); (1, 0) ]

let (++) (a, b) (x, y) = (a + x, b + y)

let get_char_at input (x,y) =
  List.nth input y |> Option.bind ~f:(fun l -> List.nth l x) |> Option.value ~default:' '

let rec string_in_direction input pos length dir = 
  if length = 0 then "" 
  else (get_char_at input pos |> String.of_char) ^ string_in_direction input (dir ++ pos) (length - 1) dir

let is_x_mas input (x,y) =
  let c = List.nth input y |> Option.bind ~f:(fun l -> List.nth l x) |> Option.value ~default:' ' in
  if Char.(c <> 'A') then false 
  else
    let lst = List.map diags ~f:(fun dir -> dir |> string_in_direction input (x, y) 2 |> String.unsafe_sub ~pos:1 ~len:1 |> Char.of_string) in
    let d1 = List.sub ~pos:0 ~len:2 lst |> Set.of_list (module Char) in
    let d2 = List.sub ~pos:2 ~len:2 lst |> Set.of_list (module Char) in
  Set.equal (Set.of_list (module Char) ['M';'S']) d1 && Set.equal (Set.of_list (module Char) ['M';'S']) d2

let () =
  let input = InputReader.read_lines ~path:"input" ~f:(String.to_list) in
  let lst1 = ref [] in

  List.iteri input ~f:(fun y line -> ( 
    List.iteri line ~f:(fun x c -> (if Char.(c = 'X') then 
      lst1 := List.append (!lst1) (List.map directions ~f:(string_in_direction input (x,y) 4))
      ))));

  List.count !lst1 ~f:(String.equal "XMAS") |> Stdio.printf "Part 1: %i\n";

  let count = ref 0 in
  List.iteri input ~f:(fun y line -> ( 
    List.iteri line ~f:(fun x _ -> (if is_x_mas input (x,y) then count := !count + 1))
    ));

  Stdio.printf "Part 2: %i\n" !count
