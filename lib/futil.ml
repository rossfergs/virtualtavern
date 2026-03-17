module Tuple = struct
  module Make (M1 : Map.OrderedType) (M2 : Map.OrderedType) = struct
    type t = M1.t * M2.t

    let compare (x0, y0) (x1, y1) =
      match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
  end
end

module Format = struct
  module Colour = struct
    type t =
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
  let log ~module_name (level : [ `info | `debug | `fatal ]) msg =
    let () =
      match level with
      | `fatal ->
          Printf.printf "[FATAL] (%s) %s\n" module_name msg;
          exit 1
      | `debug -> Printf.printf "[BEDUG] (%s) %s\n" module_name msg
      | `info -> Printf.printf "[INFO] (%s) %s\n" module_name msg
    in
    flush stdout

  let make_logger module_name = log ~module_name

  let fatal (message : string) : unit =
    print_string "[FATAL] ";
    print_endline message;
    exit 1

  let debug (message : string) : unit =
    print_string "[BEDUG] ";
    print_endline message

  let info (message : string) : unit =
    print_string "[INFO] ";
    print_endline message
end

module Int_tuple_map = Map.Make (Tuple.Make (Int) (Int))
module String_map = Map.Make (String)

let rand_from_list = function
  | [] -> raise (Invalid_argument "empty list given to Futil.rand_from_list")
  | ls -> List.nth ls (Random.int (List.length ls))

let rand_from_list_opt = function
  | [] -> None
  | ls -> Some (List.nth ls (Random.int (List.length ls)))

let range s e =
  let rec range_impl current acc =
    if current = e then List.rev acc
    else range_impl (current + 1) (current :: acc)
  in
  range_impl s []

let ( -- ) s e = range s e

let read_lines filename : string list =
  In_channel.with_open_text filename In_channel.input_lines
