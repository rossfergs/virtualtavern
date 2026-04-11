module Int_tuple_set = Set.Make (Futil.Tuple.Make (Int) (Int))

let log = Futil.Logging.log ~tag:__MODULE__ ~min_level:`trace
let h = Futil.Math.get_linear_distance

let g (start_x, start_y) (next_x, next_y) =
  let delta_x, delta_y = (abs (start_x - next_x), abs (start_y - next_y)) in
  if delta_x > 1 || delta_y > 1 then
    raise
      (Invalid_argument
         "g score can only be recorded for values next to reachother")
  else if (delta_x = 0 && delta_y = 1) || (delta_x = 1 && delta_x = 0) then 10
  else 14

let f current next end_node = g current next + h next end_node

let get_ordered_nodes current nodes end_node =
  Futil.quicksort
    (fun a b -> f current a end_node >= f current b end_node)
    nodes

let get_neighbours f1 f2 (x, y) =
  [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ]
  @ [ (x - 1, y); (x + 1, y) ]
  @ [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
  |> List.filter f1 |> List.filter f2

let find_path ?(is_valid = fun _n -> true) start_node end_node =
  let ex, ey = end_node in
  let rec a_star_impl path open_set closed_set =
    match open_set with
    | [] -> ([], closed_set)
    | [ node ] -> (
        let sx, sy = node in
        log `trace (Printf.sprintf "(%d, %d) -> (%d, %d)" sx sy ex ey);
        if node = end_node then (List.rev (node :: path), closed_set)
        else
          match
            get_ordered_nodes node
              (get_neighbours is_valid
                 (fun n -> not @@ Int_tuple_set.mem n closed_set)
                 node)
              end_node
          with
          | [] -> ([], closed_set)
          | n :: ns -> (
              match
                a_star_impl (node :: path) [ n ]
                  (Int_tuple_set.add node closed_set)
              with
              | [], cs -> a_star_impl path ns cs
              | p, cs -> (p, cs)))
    | n :: ns -> (
        let sx, sy = n in
        log `trace (Printf.sprintf "(%d, %d) -> (%d, %d)" sx sy ex ey);
        if n = end_node then (List.rev (n :: path), closed_set)
        else
          match a_star_impl path [ n ] (Int_tuple_set.add n closed_set) with
          | [], cs -> a_star_impl path ns cs
          | p, cs -> (p, cs))
  in
  match a_star_impl [] [ start_node ] Int_tuple_set.empty with
  | n :: ns, _ when n = start_node -> ns
  | ns, _ -> ns
