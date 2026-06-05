let log = Futil.Logging.log ~tag:__MODULE__ ~min_level:`error

type obj = Chair | Stool | Table | Door | Bar

type tile =
  | Grass of { symbol : char; occupant : Person.t option }
  | Floor of Person.t option * obj option
  | Wall

type t = tile Futil.Int_tuple_map.t

let max_x = 24
let max_y = 25

let all_coords =
  let open Futil in
  List.map (fun y -> List.map (fun x -> (y, x)) (0 -- max_y)) (0 -- max_x)

let char_of_tile tile =
  match tile with
  | Grass { occupant = Some _; _ } -> '@'
  | Grass { symbol; _ } -> symbol
  | Floor _ -> '+'
  | Wall -> '#'

let add_people people tavern =
  let add_person person tile =
    match tile with
    | Grass { symbol; occupant = None } ->
        Grass { symbol; occupant = Some person }
    | Floor (None, o) -> Floor (Some person, o)
    | _ -> raise (Invalid_argument "tile must be an empty space in add_person")
  in
  let coords = List.map (fun p -> p.Person.location) people in
  let tiles =
    List.map2 (fun c p -> (p, Futil.Int_tuple_map.find c tavern)) coords people
  in
  List.fold_left2
    (fun acc c (p, t) -> Futil.Int_tuple_map.add c (add_person p t) acc)
    tavern coords tiles

let string_of_obj obj =
  let open Futil.Format in
  match obj with
  | Chair -> format_string [ FG_Colour Light_brown ] "o"
  | Table -> format_string [ FG_Colour Light_brown ] "O"
  | Stool -> format_string [ FG_Colour Red ] "*"
  | Door -> format_string [ BG_Colour Brown; FG_Colour Grey; Bold ] ":"
  | Bar -> format_string [ BG_Colour Brown; FG_Colour Light_brown ] "="

let string_of_tile tile =
  let open Futil.Format in
  match tile with
  | Grass { occupant = Some _; _ } -> "@"
  | Grass { symbol; _ } ->
      format_string [ Italic; FG_Colour Green ] (String.make 1 symbol)
  | Floor (Some _, _) -> "@"
  | Floor (None, Some obj) -> string_of_obj obj
  | Floor (None, None) -> format_string [ FG_Colour Brown ] "+"
  | Wall -> format_string [ Bold; BG_Colour Grey; FG_Colour Black ] "+"

let string_of_tavern tavern =
  let tavern_list =
    List.map
      (fun c -> List.map (fun idk -> Futil.Int_tuple_map.find idk tavern) c)
      all_coords
  in
  List.fold_left
    (fun acc r ->
      (acc ^ List.fold_left (fun acc tile -> acc ^ string_of_tile tile) "" r)
      ^ "\n")
    "" tavern_list

let print_tavern (tavern : t) : unit =
  let open Futil in
  log `debug "starting print";
  all_coords
  |> List.iter (fun col ->
      List.iter
        (fun coord ->
          print_string @@ string_of_tile @@ Int_tuple_map.find coord tavern)
        col;
      print_newline ())

let get_filled_rect_coords (tx, ty) width height =
  Futil.range tx (tx + width)
  |> List.map (fun x -> List.map (fun y -> (x, y)) Futil.(ty -- (ty + height)))
  |> List.flatten

let get_rect_coords (tx, ty) width height =
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

let new_grass () =
  match Random.int 6 with
  | 0 -> Grass { symbol = ','; occupant = None }
  | 1 -> Grass { symbol = '.'; occupant = None }
  | 2 -> Grass { symbol = '\''; occupant = None }
  | 3 -> Grass { symbol = '"'; occupant = None }
  | _ -> Grass { symbol = ' '; occupant = None }

let move_person person dest_coord tavern : t =
  let px, py = person.Person.location in
  let dx, dy = dest_coord in
  log `debug
    (Printf.sprintf "moving person from (%d, %d) to (%d, %d)" px py dx dy);
  let open Futil.Int_tuple_map in
  let open Person in
  match (find person.location tavern, find dest_coord tavern) with
  | Floor (Some _, obj1), Floor (None, obj2) ->
      add person.location (Floor (None, obj1)) tavern
      |> add dest_coord (Floor (Some person, obj2))
  | Grass { symbol; occupant = Some _ }, Floor (None, obj) ->
      add person.location (Grass { symbol; occupant = None }) tavern
      |> add dest_coord (Floor (Some person, obj))
  | Floor (Some _, obj), Grass { symbol; occupant = None } ->
      add person.location (Floor (None, obj)) tavern
      |> add dest_coord (Grass { symbol; occupant = Some person })
  | ( Grass { symbol = sym1; occupant = Some _ },
      Grass { symbol = sym2; occupant = None } ) ->
      add person.location (Grass { symbol = sym1; occupant = None }) tavern
      |> add dest_coord (Grass { symbol = sym2; occupant = Some person })
  | l, r ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "move_person can only move between passable nodes. current nodes \
               %c and %c"
              (char_of_tile l) (char_of_tile r)))

let random_tile ?(coord_list = List.flatten all_coords)
    ?(qualifier = fun _ -> true) tavern =
  coord_list
  |> List.filter (fun c -> qualifier (Futil.Int_tuple_map.find c tavern))
  |> Futil.rand_from_list

let random_free_floor tavern =
  random_tile
    ~qualifier:(fun n ->
      match n with
      | Floor (None, None) | Floor (None, Some Chair) -> true
      | _ -> false)
    tavern

let random_stool tavern =
  random_tile
    ~qualifier:(fun n ->
      match n with Floor (None, Some Stool) -> true | _ -> false)
    tavern

let random_edge_tile tavern =
  let edge_coords = get_rect_coords (0, 0) (max_x - 1) (max_y - 1) in
  random_tile ~coord_list:edge_coords
    ~qualifier:(fun n -> match n with Grass _ -> true | _ -> false)
    tavern

let is_occupied = function
  | Floor (Some _, _) | Grass { occupant = Some _; _ } -> true
  | _ -> false

let is_passable = function
  | Floor (None, None)
  | Floor (None, Some Door)
  | Floor (None, Some Stool)
  | Grass { occupant = None; _ } ->
      true
  | _ -> false

let find_path (sx, sy) (ex, ey) t =
  log `debug
    (Printf.sprintf "finding path from (%d, %d) to (%d, %d)" sx sy ex ey);
  Astar.find_path
    ~is_valid:(fun (nx, ny) ->
      ((0 <= nx && nx <= max_x - 1) && 0 <= ny && ny <= max_y - 1)
      && (is_passable @@ Futil.Int_tuple_map.find (nx, ny) t))
    (sx, sy) (ex, ey)

(* INCREDIBLY bullshit initial tavern *)
let init_tavern () : t =
  (*  let open Futil in *)
  log `debug "starting initialisation";
  let top_x, top_y = (max_x / 4, max_y / 4) in
  let _mid_x, _mid_y = (max_x / 2, max_y / 2) in
  let wall_width, wall_height = ((max_x / 2) - 1, max_y / 2) in
  log `debug "generating grass";
  let grass = List.map (fun c -> (c, new_grass ())) (List.flatten all_coords) in
  log `debug "generating walls";
  let walls =
    List.map
      (fun c -> (c, Wall))
      (get_rect_coords (top_x, top_y) wall_width wall_height)
  in
  log `debug "generating floor";
  let floor =
    List.map
      (fun c -> (c, Floor (None, None)))
      (get_filled_rect_coords
         (top_x + 1, top_y + 1)
         (wall_width - 1) (wall_height - 1))
  in
  log `debug "generating doors";
  let doors =
    List.map
      (fun c -> (c, Floor (None, Some Door)))
      (get_filled_rect_coords (top_x, top_y + 1) 1 2)
  in
  log `debug "generating bar";
  let bar =
    List.map
      (fun c -> (c, Floor (None, Some Bar)))
      (get_filled_rect_coords (top_x + 1, top_y + 9) 7 1)
  in
  log `debug "generating tables and chairs";
  (* tac = tables_and_chairs *)
  let tac1 =
    List.map
      (fun c -> (c, Floor (None, Some Table)))
      (get_filled_rect_coords (top_x + 9, top_y + 4) 2 2)
    @ List.map
        (fun c -> (c, Floor (None, Some Chair)))
        (get_filled_rect_coords (top_x + 9, top_y + 6) 2 1)
    @ List.map
        (fun c -> (c, Floor (None, Some Chair)))
        (get_filled_rect_coords (top_x + 9, top_y + 3) 2 1)
  in
  let tac2 =
    List.map
      (fun c -> (c, Floor (None, Some Table)))
      (get_filled_rect_coords (top_x + 5, top_y + 1) 2 2)
    @ List.map
        (fun c -> (c, Floor (None, Some Chair)))
        (get_filled_rect_coords (top_x + 4, top_y + 1) 1 2)
    @ List.map
        (fun c -> (c, Floor (None, Some Chair)))
        (get_filled_rect_coords (top_x + 7, top_y + 1) 1 2)
  in
  let tac3 =
    List.map
      (fun c -> (c, Floor (None, Some Table)))
      (get_filled_rect_coords (top_x + 4, top_y + 5) 2 2)
    @ List.map
        (fun c -> (c, Floor (None, Some Chair)))
        (get_filled_rect_coords (top_x + 3, top_y + 5) 1 2)
    @ List.map
        (fun c -> (c, Floor (None, Some Chair)))
        (get_filled_rect_coords (top_x + 6, top_y + 5) 1 2)
  in
  log `debug "making stools";
  let stools =
    List.map
      (fun c -> (c, Floor (None, Some Stool)))
      (get_filled_rect_coords (top_x + 1, top_y + 8) 8 1)
    |> List.filter (fun ((x, _), _) -> Futil.Math.relative_even x (top_x + 1))
  in
  log `debug "putting it all together";
  let open Futil.Int_tuple_map in
  (* empty |> add_list grass |> add_list walls |> add_list floor |> add_list bar *)
  (* |> add_list doors |> add_list tac1 |> add_list tac2 |> add_list tac3 *)
  (* |> add_list stools |> add_list path *)
  let final_tavern =
    List.fold_left
      (fun acc c -> add_list c acc)
      empty
      [ grass; walls; floor; bar; doors; tac1; tac2; tac3; stools ]
  in
  log `debug "testing A-Star...";
  let path =
    find_path (max_x - 1, max_y - 1) (max_x / 2, max_y / 2) final_tavern
  in
  log `debug
    (Printf.sprintf "Path found... length of path = %d" (List.length path));
  let path = List.map (fun c -> (c, Floor (None, Some Stool))) path in
  add_list path final_tavern
