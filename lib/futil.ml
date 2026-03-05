module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

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

let rand_from_list = function
  | [] -> None
  | [ _ ] -> None
  | ls ->
      Some (List.nth ls (Random.int_in_range ~min:0 ~max:(List.length ls - 1)))

let ( -- ) s e =
  let rec range_impl current acc =
    if current = e then List.rev acc
    else range_impl (current + 1) (current :: acc)
  in
  range_impl s []

let read_lines filename : string list =
  In_channel.with_open_text filename In_channel.input_lines
