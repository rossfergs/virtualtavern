module Math = struct
  (* moronic function to get evens from a starting point *)
  let relative_even number start =
    (number mod 2 = 0 && start mod 2 = 0)
    || (number mod 2 = 1 && start mod 2 = 1)

  let pythag a b =
    int_of_float @@ Float.ceil @@ Float.mul 10.0 @@ sqrt
    @@ float_of_int ((a * a) + (b * b))

  let get_linear_distance (x1, y1) (x2, y2) =
    let x_len = abs (x1 - x2) in
    let y_len = abs (y1 - y2) in
    pythag x_len y_len
end

module Tuple = struct
  module Make (M1 : Map.OrderedType) (M2 : Map.OrderedType) = struct
    type t = M1.t * M2.t

    let compare (x0, y0) (x1, y1) =
      match M1.compare x0 x1 with 0 -> M2.compare y0 y1 | c -> c
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
      if List.length !previous_logs > 10 then previous_logs := []
      else previous_logs := msg :: !previous_logs;
      print_string msg;
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

let string_of_chars = List.fold_left (fun acc c -> acc ^ String.make 1 c) ""

let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  close_in ic;
  result

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
  type option = Bordered | Title of string
  type cell = { sym : char; style : string }

  type window = {
    lines : cell array array;
    x : int;
    y : int;
    width : int;
    height : int;
  }

  type screen = { windows : window list; max_width : int; max_height : int }

  let parse_line line : cell array =
    let rec collect_chars i cell_acc current_style =
      if i >= String.length line then Array.of_list (List.rev cell_acc)
      else
        match line.[i] with
        | '\027' -> collect_style (i + 1) cell_acc [ '\027' ] current_style
        | ch ->
            collect_chars (i + 1)
              ({ sym = ch; style = current_style } :: cell_acc)
              current_style
    and collect_style i cell_acc style_acc current_style =
      if i >= String.length line then Array.of_list (List.rev cell_acc)
      else
        match line.[i] with
        | 'm' ->
            let style = string_of_chars (List.rev ('m' :: style_acc)) in
            let new_style =
              if style = "\027[0m" then "" else current_style ^ style
            in
            collect_chars (i + 1) cell_acc new_style
        | ch -> collect_style (i + 1) cell_acc (ch :: style_acc) current_style
    in
    collect_chars 0 [] ""

  let border_window w =
    let horiz_border = parse_line ("o" ^ String.make w.width '-' ^ "o") in
    let edge = { sym = '|'; style = "" } in
    let new_lines =
      Array.make (Array.length w.lines + 2) [| { sym = ' '; style = "" } |]
    in
    for i = 1 to Array.length new_lines - 2 do
      new_lines.(i) <-
        Array.concat
          [
            [| edge |];
            w.lines.(i - 1);
            Array.make
              (w.width - Array.length w.lines.(i - 1))
              { sym = ' '; style = "" };
            [| edge |];
          ]
    done;
    new_lines.(0) <- horiz_border;
    new_lines.(Array.length new_lines - 1) <- horiz_border;
    {
      lines = new_lines;
      width = w.width;
      x = w.x;
      y = w.y;
      height = Array.length new_lines;
    }

  let make_window ?(bordered = false) ?(_title = "") input x y =
    let lines =
      Array.map parse_line (Array.of_list @@ String.split_on_char '\n' input)
    in
    let width =
      Array.fold_left
        (fun acc n -> if Array.length n > acc then Array.length n else acc)
        0 lines
    in
    if not bordered then { lines; width; x; y; height = Array.length lines }
    else border_window { lines; width; x; y; height = Array.length lines }

  let empty =
    {
      windows = [];
      max_width = int_of_string @@ run_cmd "tput cols";
      max_height = (int_of_string @@ run_cmd "tput lines") - 1;
    }

  let add w s =
    let windows = s.windows in
    { s with windows = w :: windows }

  let get_char_from_window w x y =
    let rel_x, rel_y = (x - w.x, y - w.y) in
    try Some w.lines.(rel_y).(rel_x) with _ -> None

  let get_furthest_point screen =
    List.fold_left
      (fun acc w -> if w.x + w.width > acc then w.x + w.width else acc)
      0 screen.windows

  let make_line screen line =
    let furthest_point =
      if get_furthest_point screen > screen.max_width then screen.max_width
      else get_furthest_point screen
    in
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
          | None -> " " :: acc
          | Some { sym; style } ->
              (style ^ String.make 1 sym ^ "\027[0m") :: acc)
        [] (0 -- furthest_point)
    in
    String.concat "" (List.rev chars)

  let is_window_on_line window line =
    line >= window.y && line < window.y + window.height

  let render_screen screen =
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
