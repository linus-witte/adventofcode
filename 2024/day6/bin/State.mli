open Base

type t = { guard : (Int.t * Int.t);  guard_dir : (Int.t * Int.t); map_size : (Int.t * Int.t); obstacles : (Int.t * Int.t) Sequence.t; }

val of_input : char List.t List.t -> t

val step : t -> t Option.t

val pos : t -> (Int.t * Int.t)

val add_obstacle : t -> (Int.t * Int.t) -> t

val id : t -> String.t

val to_string : t -> String.t
