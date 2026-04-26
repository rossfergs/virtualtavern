module Math = struct
  (* moronic function to get evens from a starting point *)
  let relative_even number start =
    (number mod 2 = 0 && start mod 2 = 0)
    || (number mod 2 = 1 && start mod 2 = 1)

  let pythag o a =
    int_of_float @@ Float.ceil @@ Float.mul 10.0 @@ sqrt
    @@ float_of_int ((o * o) + (a * a))

  let get_linear_distance (x1, y1) (x2, y2) =
    let x_len = abs x1 - x2 in
    let y_len = abs y1 - y2 in
    pythag x_len y_len
end

module Tuple = struct
  module Make (M1 : Map.OrderedType) (M2 : Map.OrderedType) = struct
    type t = M1.t * M2.t

    let compare (x0, y0) (x1, y1) =
      match M1.compare x0 x1 with 0 -> M2.compare y0 y1 | c -> c
  end
end

module Heap = struct
  module Make (M : Map.OrderedType) = struct
    type t = Node of M.t * t * t | Empty

    let is_empty = function Empty -> true | _ -> false

    let rec size heap =
      match heap with Empty -> 0 | Node (_, l, r) -> 1 + size l + size r

    let rec _mem value heap =
      match heap with
      | Empty -> false
      | Node (node_value, l, r) ->
          if node_value = value then true
          else if node_value > value then false
          else if _mem value l then true
          else _mem value r
    (* 
    let heapify start_heap ~comparitor =
      let rec aux heap =
        match heap with Empty -> Empty | Node (v, l, r) -> Empty
      in
      aux start_heap
      *)

    let rec pop ~comparitor = function
      | Empty -> Empty
      | Node (_, l, r) -> (
          match (l, r) with
          | Empty, _ -> r
          | _, Empty -> l
          | Node (lv, _, _), Node (rv, _, _) when comparitor lv rv ->
              Node (lv, pop l ~comparitor, r)
          | Node (_, _, _), Node (rv, _, _) -> Node (rv, l, pop r ~comparitor))

    (*
    let delete value start_heap ~comparitor =
      if not (mem value start_heap) then raise (Invalid_argument "value not found in heap when deleting") else
      let rec delete_aux heap found =
        match heap with
        | Empty -> Empty
        | Node (nv, lh, lr) ->
            let found = nv = value in
            let parent_val, child_val = if found then nv, value else value nv in
            if found then
              (match delete_aux lh found with
                | Node (cnv, _, _) when comparitor nv cnv
                | n -> n
              )
      in
      delete_aux start_heap
      *)

    let rec insert new_value heap ~comparitor =
      match heap with
      | Empty -> Node (new_value, Empty, Empty)
      | Node (node_value, lh, rh) -> (
          let pv, cv =
            if comparitor new_value node_value then (new_value, node_value)
            else (node_value, new_value)
          in
          match (lh, rh) with
          | Empty, _ -> Node (pv, Node (cv, Empty, Empty), rh)
          | _, Empty -> Node (pv, lh, Node (cv, Empty, Empty))
          | Node _, Node _ ->
              if size lh < size rh then Node (pv, insert cv lh ~comparitor, rh)
              else Node (pv, lh, insert cv rh ~comparitor))

    let peek = function
      | Empty -> raise (Invalid_argument "Trying to peek into empty heap")
      | Node (v, _, _) -> v

    let peek_opt = function Empty -> None | Node (v, _, _) -> Some v

    let rec map func heap =
      match heap with
      | Empty -> Empty
      | Node (v, l, r) -> Node (func v, map func l, map func r)

    let rec mem value heap =
      match heap with
      | Empty -> false
      | Node (node_value, l, r) ->
          if node_value = value then true
          else if node_value > value then false
          else if mem value l then true
          else mem value r
  end

  module Min_Heap (M : Map.OrderedType) = struct
    include Make (M)

    let comparitor = fun a b -> M.compare a b <= 0
    let pop = pop ~comparitor
    let insert = insert ~comparitor
  end

  module Max_Heap (M : Map.OrderedType) = struct
    include Make (M)

    let insert = insert ~comparitor:(fun a b -> M.compare a b >= 0)
  end
end

module Format = struct
  module Colour = struct
    type t =
      | Brick
      | White
      | Black
      | Yellow
      | Green
      | Blue
      | Red
      | Brown
      | Light_brown
      | Grey

    let to_int = function
      | Brick -> 167
      | White -> 7
      | Black -> 16
      | Yellow -> 3
      | Green -> 28
      | Blue -> 4
      | Red -> 1
      | Brown -> 94
      | Light_brown -> 136
      | Grey -> 8
  end

  type operation =
    | Bold
    | Underlined
    | Italic
    | BG_Colour of Colour.t
    | FG_Colour of Colour.t

  let colour_string ground str num =
    Printf.sprintf "\027[%d;5;%dm%s" ground num str

  let add_suffix oper msg =
    match oper with
    | Bold -> "\027[1m" ^ msg
    | Underlined -> "\027[4m" ^ msg
    | Italic -> "\027[3m" ^ msg
    | FG_Colour c -> colour_string 38 msg (Colour.to_int c)
    | BG_Colour c -> colour_string 48 msg (Colour.to_int c)

  let rec format_string opers msg =
    match opers with
    | [] -> msg ^ "\027[0m"
    | o :: os -> format_string os (add_suffix o msg)
end

module Logging = struct
  let logs_on = ref true
  let set_logging_on b = logs_on := b
  let previous_logs = ref []

  type level = [ `trace | `debug | `info | `error | `fatal ]

  let module_name_prefix = "Virtualtavern__"

  let trim_module_name name =
    if not (String.starts_with ~prefix:module_name_prefix name) then name
    else
      let length_of_prefix = String.length module_name_prefix in
      String.sub name length_of_prefix (String.length name - length_of_prefix)

  let log ?(min_level : level = `info) ?(tag = "") (level : level) msg =
    if not !logs_on then ()
    else
      let level_to_int (l : level) =
        match l with
        | `fatal -> 10
        | `error -> 8
        | `info -> 5
        | `debug -> 3
        | `trace -> 1
      in
      let msg =
        if level_to_int min_level > level_to_int level then ""
        else
          let print_fmt =
           fun level_tag ->
            Printf.sprintf "%s |%s| %s\n" level_tag (trim_module_name tag) msg
          in
          match level with
          | `fatal ->
              print_fmt
                (Format.format_string
                   [ BG_Colour Red; FG_Colour Black ]
                   "[FATAL]")
          | `error ->
              print_fmt (Format.format_string [ FG_Colour Red ] "[ERROR]")
          | `info ->
              print_fmt (Format.format_string [ FG_Colour Green ] "[INFO]")
          | `debug ->
              print_fmt (Format.format_string [ FG_Colour Yellow ] "[BEDUG]")
          | `trace ->
              print_fmt (Format.format_string [ FG_Colour Grey ] "[TRACE]")
      in
      print_string msg;
      if List.length !previous_logs >= 10 then previous_logs := []
      else previous_logs := msg :: !previous_logs;
      flush stdout
end

module Int_tuple_map = struct
  include Map.Make (Tuple.Make (Int) (Int))

  let rec add_list l m =
    match l with [] -> m | (k, v) :: xs -> add_list xs (add k v m)
end

module String_map = struct
  include Map.Make (String)

  let rec add_list l m =
    match l with [] -> m | (k, v) :: xs -> add_list xs (add k v m)
end

let rand_from_list = function
  | [] -> raise (Invalid_argument "empty list given to Futil.rand_from_list")
  | ls -> List.nth ls (Random.int (List.length ls))

let rand_from_list_opt = function
  | [] -> None
  | ls -> Some (List.nth ls (Random.int (List.length ls)))

let range ?(inc = 1) s e =
  let rec range_impl current acc =
    if current >= e then List.rev acc
    else range_impl (current + inc) (current :: acc)
  in
  range_impl s []

let ( -- ) = range

let read_lines filename : string list =
  In_channel.with_open_text filename In_channel.input_lines

let quicksort comparitor list =
  let rec quicksort_impl = function
    | [] -> []
    | x :: xs ->
        let left, right = List.partition (comparitor x) xs in
        quicksort_impl left @ [ x ] @ quicksort_impl right
  in
  quicksort_impl list

module Windower = struct
  type window = {
    lines : string list;
    x : int;
    y : int;
    width : int;
    height : int;
  }

  type screen = { windows : window list; max_width : int; max_height : int }

  let create_window input x y =
    let lines = String.split_on_char '\n' input in
    let width =
      List.fold_left
        (fun acc n -> if String.length n > acc then String.length n else acc)
        0 lines
    in
    { lines; width; x; y; height = List.length lines }

  let empty =
    let run_cmd cmd =
      let ic = Unix.open_process_in cmd in
      let result = input_line ic in
      close_in ic;
      result
    in
    let _ = run_cmd "clear" in
    {
      windows = [];
      max_width = (int_of_string @@ run_cmd "tput cols") / 2;
      max_height = (int_of_string @@ run_cmd "tput lines") - 1;
    }

  let add w s =
    let windows = s.windows in
    { s with windows = w :: windows }

  let get_char_from_window w x y =
    let rel_x, rel_y = (x - w.x, y - w.y) in
    if rel_x >= w.width || rel_y >= w.height || rel_x < 0 || rel_y < 0 then None
    else try Some (String.get (List.nth w.lines rel_y) rel_x) with _ -> None

  let get_furthest_point screen =
    List.fold_left
      (fun acc w -> if w.x + w.width > acc then w.x + w.width else acc)
      0 screen.windows

  let make_line screen line =
    let stringify = List.fold_left (fun acc c -> acc ^ String.make 1 c) "" in
    (*    let find_character windows x y =  *)
    (*          List.fold_left *)
    (*            (fun acc w -> *)
    (*              let ch = get_char_from_window w col line in *)
    (*              if ch = None then acc else ch) *)
    (*            None windows *)
    (*      if col >= screen.max_width || col >= furthest_point then *)
    (*        stringify (List.rev acc) *)
    (*      else  *)
    let chars =
      List.fold_left
        (fun acc col ->
          match
            List.fold_left
              (fun acc w ->
                let ch = get_char_from_window w col line in
                if ch = None then acc else ch)
              None screen.windows
          with
          | None -> '#' :: acc
          | Some ch -> ch :: acc)
        []
        (0 -- get_furthest_point screen)
    in
    stringify (List.rev chars)

  let render_screen screen =
    let is_window_on_line window line =
      line >= window.y && line <= window.y + window.height
    in
    let last_line =
      List.fold_left
        (fun acc w -> if w.y + w.height > acc then w.y + w.height else acc)
        0 screen.windows
    in
    let rec impl lines_acc line =
      if line >= screen.max_height || line >= last_line then
        List.fold_left (fun acc l -> acc ^ l ^ "\n") "" (List.rev lines_acc)
      else if
        List.for_all (fun w -> not (is_window_on_line w line)) screen.windows
      then impl lines_acc (line + 1)
      else impl (make_line screen line :: lines_acc) (line + 1)
    in
    impl [] 0
end
