open Util

let make_person_list (length : int) : Person.t list =
  let range = 1 -- (length + 1) in
  let names = List.map (fun num -> "Person " ^ string_of_int num) range in
  let people = List.map (fun name -> Person.make_person name) names in
  people

let run_manager () : unit =
  let people = make_person_list 3 in
  let announce_and_update =
   fun p ->
    Util.info (Person.message_string_of_person p);
    Unix.sleepf 0.25;
    Person.update_activity p
  in
  let rec loop pl = loop (List.map announce_and_update pl) in
  loop people
