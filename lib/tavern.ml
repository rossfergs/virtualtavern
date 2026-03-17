(*
  Note on the current implementation:
    coordinates are not intuitive, below shows how coords are printed

     0  y   7
    o-------*
   0|
    |   @
   x|   ^
    |   |
    |   |
   7*   |
        |
        |
  @ is positioned
     at (1, 2)

  Actually, this isnt accurate anymore, just
  keeping it cause i like the visualisiaton
*)

type obj = Chair | Stool | Table | Door | Bar

let string_of_obj obj =
  let open Futil.Format in
  match obj with
  | Chair -> format_string [ FG_Colour Light_brown ] "o"
  | Table -> format_string [ FG_Colour Light_brown ] "O"
  | Stool -> format_string [ FG_Colour Light_brown ] "*"
  | Door -> format_string [ BG_Colour Brown; FG_Colour Grey; Bold ] ":"
  | Bar -> format_string [ BG_Colour Brown; FG_Colour Light_brown ] "="

type tile =
  | Grass of { symbol : char; occupant : Person.t option }
  | Floor of Person.t option * obj option
  | Wall

type t = tile Futil.Int_tuple_map.t

let max_x = 24
let max_y = 24

let new_grass () =
  match Random.int 6 with
  | 0 -> Grass { symbol = ','; occupant = None }
  | 1 -> Grass { symbol = '.'; occupant = None }
  | 2 -> Grass { symbol = '\''; occupant = None }
  | 3 -> Grass { symbol = '"'; occupant = None }
  | _ -> Grass { symbol = ' '; occupant = None }

let char_of_tile tile =
  match tile with Grass { symbol; _ } -> symbol | Floor _ -> '+' | Wall -> '#'

let string_of_tile tile =
  let open Futil.Format in
  match tile with
  | Grass { symbol; _ } ->
      format_string [ Italic; FG_Colour Green ] (String.make 1 symbol)
  | Floor (Some _, _) -> format_string [ FG_Colour White ] "@"
  | Floor (None, Some obj) -> string_of_obj obj
  | Floor (None, None) -> format_string [ FG_Colour Brown ] "+"
  | Wall -> format_string [ BG_Colour Grey; FG_Colour Black ] "+"

let get_filled_rect_coords (tx, ty) height width =
  Futil.range tx (tx + width)
  |> List.map (fun x -> List.map (fun y -> (x, y)) Futil.(ty -- (ty + height)))
  |> List.flatten

let get_rect_coords (tx, ty) height width =
  (*
    Constructs flat list of coordinates 
    by generating the lines of the edges 
    and appending them together
  *)
  let open Futil in
  List.map (fun x -> (x, ty)) (ty -- (ty + height))
  @ List.map (fun x -> (x, ty + height)) (ty -- (ty + height))
  @ List.map (fun y -> (tx, y)) (ty -- (ty + height))
  @ List.map (fun y -> (tx + width, y)) (ty -- (ty + height))
  @ [ (tx + width, ty + height) ]

let init_tavern () : t =
  let open Futil in
  let top_x, top_y = (max_x / 4, max_y / 4) in
  let mid_x, mid_y = (max_x / 2, max_y / 2) in
  let wall_width, wall_height = ((max_x / 2) - 1, (max_y / 2) - 1) in
  let init_map =
    List.map
      (fun x -> List.map (fun y -> ((x, y), new_grass ())) (0 -- max_y))
      (0 -- max_x)
    |> List.flatten |> Int_tuple_map.of_list
  in
  let walls_coords =
    List.map
      (fun c -> (c, Wall))
      (get_rect_coords (top_x, top_y) wall_width wall_height)
  in
  let floor_coords =
    List.map
      (fun c -> (c, Floor (None, None)))
      (get_filled_rect_coords
         (top_x + 1, top_y + 1)
         (wall_width - 1) (wall_height - 1))
  in
  let doors_coords =
    List.map
      (fun c -> (c, Floor (None, Some Door)))
      (get_filled_rect_coords (top_x + 1, top_y) 2 1)
  in
  let bar_coords =
    List.map
      (fun c -> (c, Floor (None, Some Bar)))
      (get_filled_rect_coords (top_x + 1, top_y + 8) 7 1)
  in
  let rec add_list (l : ((int * int) * tile) list) (m : t) : t =
    match l with
    | [] -> m
    | (k, v) :: xs -> add_list xs (Futil.Int_tuple_map.add k v m)
  in
  init_map |> add_list walls_coords |> add_list floor_coords
  |> add_list bar_coords |> add_list doors_coords
  |> Int_tuple_map.add (mid_x, mid_x)
       (Floor (Some (Person.make_person "test"), None))
  |> Int_tuple_map.add (mid_x + 2, mid_y) (Floor (None, Some Chair))
  |> Int_tuple_map.add (mid_x + 2, mid_y) (Floor (None, Some Table))
  |> Int_tuple_map.add (mid_x + 3, mid_y) (Floor (None, Some Stool))

let print_tavern (tavern : t) : unit =
  let open Futil in
  List.map (fun x -> List.map (fun y -> (y, x)) (0 -- max_y)) (0 -- max_x)
  |> List.iter (fun col ->
      List.iter
        (fun coord ->
          print_string @@ string_of_tile @@ Int_tuple_map.find coord tavern)
        col;
      print_newline ())
