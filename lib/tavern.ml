(*

 THIS FILE IN UNUSED.
 To be compiled, add 'tavern' to the 'libraries' tag in dune

*)

type tile =
  | Grass of { symbol : char; occupant : Person.t option }
  | Floor of Person.t option
  | Wall
  | Bar

type t = string Futil.String_map.t

let initial_grid = 
  let open Futil in
  let max_x = 20 in
  let max_y = 20 in
  let coord_tuples = List.map (fun x -> List.map (fun y -> x, y) (0 -- max_y)) (0 -- max_x) in
