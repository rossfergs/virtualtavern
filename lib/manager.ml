module State = Futil.String_map

let find_target people : Person.t option =
  let choice =
    Futil.rand_from_list (List.filter Person.is_socialising people)
  in
  let () =
    match choice with
    | None -> Futil.debug "None"
    | Some p -> Futil.debug (Printf.sprintf "Choice: %s" p.name)
  in
  choice

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

let pick_conversation (conversations : Person.t list) : (string * string) option
    =
  let open Person in
  match conversations with
  | {
      name;
      current_activity = Socialising (Some (Seeking_Conversation { topic; _ }));
      _;
    }
    :: _ ->
      Some (name, topic)
  | _ -> None

let rec manage_no_conversation (person : Person.t)
    (other_people : Person.t list) : Conversation.t option =
  let incoming_conversations =
    List.filter (fun p -> Person.are_players_conversing person p) other_people
  in
  match pick_conversation incoming_conversations with
  | Some (name, topic) ->
      Some (In_Conversation { partner = name; topic; length = 0 })
  | None ->
      let do_nothing_chance = Random.int 3 in
      if do_nothing_chance = 0 then None
      else 
              let peoples_names = List.map (fun p -> p.Person.name) other_people in
        manage_conversation
          {
            person with
            current_activity =
              Socialising
                (Some
                   (Seeking_Conversation { target = None; topic = Topic.generate_topic peoples_names }));
                   
          }
          other_people

and manage_conversation (person : Person.t) (other_people : Person.t list) :
    Conversation.t option =
  let open Person in
  let open Conversation in
  let conv_info =
    match person.current_activity with
    | Socialising (Some c) -> c
    | _ ->
        Futil.fatal "INCORRECT LOGIC IN CONVERSATION MANAGER";
        raise (Failure "See logs for more details")
  in
  match conv_info with
  | Seeking_Conversation { target = None; topic } -> (
      match find_target other_people with
      | Some new_target ->
          Some (Seeking_Conversation { target = Some new_target.name; topic })
      | None -> None)
  | Seeking_Conversation { target = Some target; _ } ->
      let active_conversations =
        List.filter
          (fun p -> Person.are_players_conversing person p)
          other_people
      in
      let updated_activity =
        try
          match List.hd active_conversations with
          | {
           current_activity =
             Socialising (Some (In_Conversation { topic; partner; _ }));
           _;
          }
            when partner = person.name ->
              Some (In_Conversation { partner = target; topic; length = 0 })
          | _ -> None
        with Failure _ -> None
      in
      if person.current_activity = Socialising None then
        manage_conversation
          { person with current_activity = Socialising updated_activity }
          other_people
      else updated_activity
  | In_Conversation _ as in_conv ->
      let active_conversations =
        List.filter
          (fun p -> Person.are_players_conversing person p)
          other_people
      in
      if List.length active_conversations = 0 then None else Some in_conv

let rec manage_people people acc : unit =
  match people with
  | person :: remaining_people ->
      Futil.info (Person.message_string_of_person person);
      Unix.sleepf 0.10;
      let updated_person =
        match Person.update_activity person with
        | { current_activity = Socialising None; _ } as p ->
            {
              p with
              current_activity =
                Socialising
                  (manage_no_conversation p
                     (List.rev_append acc remaining_people));
            }
        | { current_activity = Socialising (Some _); _ } as p ->
            {
              p with
              current_activity =
                Socialising
                  (manage_conversation p
                     (List.append remaining_people (List.rev acc)));
            }
        | p -> p
      in
      manage_people remaining_people (updated_person :: acc)
  | [] -> manage_people (List.rev acc) []

let run_manager () : unit =
  let people = make_person_list 25 in
  if List.length people = 0 then Futil.fatal "Population cannot be 0"
  else manage_people people []
