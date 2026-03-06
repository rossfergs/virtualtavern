(*

 THIS FILE IN UNUSED.
 To be compiled, add 'tavern' to the 'libraries' tag in dune

*)

type tile =
  | Grass of { symbol : char; occupant : Person.t option }
  | Floor of Person.t ref option
  | Wall
  | Bar

let new_grass () =
  match Random.int 10 with
  | 0 -> Grass { symbol = ','; occupant = None }
  | 1 -> Grass { symbol = '.'; occupant = None }
  | 2 -> Grass { symbol = '\''; occupant = None }
  | 3 -> Grass { symbol = '"'; occupant = None }
  | _ -> Grass { symbol = ' '; occupant = None }

let char_of_tile tile =
  match tile with
  | Grass { symbol; _ } -> symbol
  | Floor _ -> '_'
  | Wall -> '#'
  | Bar -> '='

type t = tile Futil.Int_tuple_map.t

let max_x = 25
let max_y = 25

let init_tavern () : t =
  let open Futil in
  let kv_pairs =
    List.map
      (fun x -> List.map (fun y -> ((x, y), new_grass ())) (0 -- max_y))
      (0 -- max_x)
    |> List.flatten
  in
  Int_tuple_map.of_list kv_pairs

let print_tavern (tavern : t) : unit =
  let open Futil in
  List.map (fun x -> List.map (fun y -> (x, y)) (0 -- max_y)) (0 -- max_x)
  |> List.iter (fun col ->
      List.iter
        (fun coord ->
          print_char @@ char_of_tile @@ Int_tuple_map.find coord tavern)
        col;
      print_newline ())
