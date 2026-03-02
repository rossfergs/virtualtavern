type conversation_info = { sender : string; target : string }

module State = Futil.String_map

let find_target people =
  let choice =
    Futil.rand_from_list (List.filter Person.is_socialising people)
  in
  let () =
    match choice with
    | None -> Futil.debug "None"
    | Some p -> Futil.debug (Printf.sprintf "Choice: %s" p.name)
  in
  choice

(* TODO: Expand this to actually pick a conversation based on interests and that *)
let pick_conversation (conversations : Person.t list) : (string * string) option
    =
  match conversations with
  | { name; current_activity = Seeking_Conversation { topic; _ }; _ } :: _ ->
      Some (name, topic)
  | _ -> None

let update_state key func state =
  let node = State.find key state in
  let new_node = func node in
  let new_state = State.add key new_node state in
  new_state

let make_person_list (length : int) : Person.t list =
  let open Futil in
  let range = 1 -- (length + 1) in
  let names = List.map (fun num -> "Person " ^ string_of_int num) range in
  let people = List.map (fun name -> Person.make_person name) names in
  people

let rec run_manager_loop people acc : unit =
  let rec manage_update (person : Person.t) (other_people : Person.t list) :
      Person.t =
    let updated_person = Person.update_activity person in
    match updated_person.current_activity with
    | Seeking_Conversation { target = None; topic } ->
        let updated_person =
          match find_target other_people with
          | None -> { person with current_activity = Person.Socialising }
          | Some new_target ->
              let new_activity =
                Person.Seeking_Conversation
                  { target = Some new_target.name; topic }
              in
              { person with current_activity = new_activity }
        in
        updated_person
    | Seeking_Conversation { target = Some target; _ } ->
        let active_conversations =
          List.filter
            (fun p -> Person.are_players_conversing person p)
            other_people
        in
        let updated_activity =
          try
            match List.hd active_conversations with
            | { current_activity = Conversation { topic; partner; _ }; _ }
              when partner = person.name ->
                Person.Conversation { partner = target; topic; length = 0 }
            | _ -> Person.Socialising
          with Failure _ -> Person.Socialising
        in
        if person.current_activity = Person.Socialising then
          manage_update
            { person with current_activity = updated_activity }
            other_people
        else { person with current_activity = updated_activity }
    | Socialising ->
        let incoming_conversations =
          List.filter
            (fun p -> Person.are_players_conversing person p)
            other_people
        in
        let output_person =
          match pick_conversation incoming_conversations with
          | Some (name, topic) ->
              {
                person with
                current_activity =
                  Person.Conversation { partner = name; topic; length = 0 };
              }
          | None ->
              let do_nothing_chance = Random.int_in_range ~min:0 ~max:2 in
              if do_nothing_chance = 0 then
                { person with current_activity = Person.Socialising }
              else
                manage_update
                  {
                    person with
                    current_activity =
                      Person.Seeking_Conversation
                        { target = None; topic = "something" };
                  }
                  other_people
        in
        output_person
    | Conversation _ ->
        let active_conversations =
          List.filter
            (fun p -> Person.are_players_conversing person p)
            other_people
        in
        if List.length active_conversations = 0 then
          { person with current_activity = Person.Socialising }
        else updated_person
    | _ -> updated_person
  in
  match people with
  | person :: remaining_people ->
      Futil.info (Person.message_string_of_person person);
      Unix.sleepf 0.01;
      let updated_person =
        manage_update person (List.append remaining_people (List.rev acc))
      in
      run_manager_loop remaining_people (updated_person :: acc)
  | [] -> run_manager_loop (List.rev acc) []

let run_manager () : unit =
  let people = make_person_list 25 in
  if List.length people = 0 then Futil.fatal "Population cannot be 0"
  else run_manager_loop people []
