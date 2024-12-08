(** 
    Reads an input files and converts its to a list.
    Each list entry corresponds to line with function f applied to it.

    @param path The path to read from
    @param f The function to be applied to each list entry
    *)
let read_lines ~path ~f =
  let ic = open_in path in
  let rec next ic =
    try
      let line = f (input_line ic) in
      line :: next ic
    with
    | End_of_file ->
        close_in ic;
        []
  in
  next ic


let read_lines_filter ~path ~f =
  let ic = open_in path in
  let rec next ic =
    try
      match f (input_line ic) with
      | Some line -> line :: next ic
      | None -> next ic
    with
    | End_of_file ->
        close_in ic;
        []
  in
  next ic
