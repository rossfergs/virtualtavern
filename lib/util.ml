let info (message : string) : unit =
  print_string "[INFO] ";
  print_endline message

let ( -- ) s e =
  let rec range_impl current acc =
    if current = e then List.rev acc
    else range_impl (current + 1) (current :: acc)
  in
  range_impl s []
