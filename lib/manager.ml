let log = Futil.Logging.log ~tag:__MODULE__ ~min_level:`info

let find_target people : Person.t option =
  let idle_people = List.filter Person.is_socialising people in
  if idle_people = [] then None else Futil.rand_from_list_opt idle_people

let initial_setup (length : int) : Person.t list * Tavern.t =
  log `debug "making initial setup";
  let tavern = Tavern.init_tavern in
  let range = Futil.(1 -- (length + 1)) in
  let names = List.map (fun num -> "Person " ^ string_of_int num) range in
  log `debug "creating list of people";
  let people =
    List.map
      (fun name -> Person.make_person name (Tavern.random_edge_tile tavern))
      names
  in
  let tavern = Tavern.add_people people tavern in
  (people, tavern)

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
                   (Seeking_Conversation
                      {
                        target = None;
                        topic = Topic.generate_topic peoples_names;
                      }));
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
        log `fatal "INCORRECT LOGIC IN CONVERSATION MANAGER";
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

let rec handle_travel person tavern : Person.t * Tavern.t =
  let open Person in
  match person.current_activity with
  | Travelling { destination; path = None } ->
      log `debug
        (Printf.sprintf "handling %s's travel: no path present" person.name);
      let destination_coords =
        match destination with
        | "floor" -> Tavern.random_free_floor tavern
        | "bar" -> Tavern.random_stool tavern
        | d ->
            log `error ("impossible destination in handle_travel: " ^ d);
            Tavern.random_free_floor tavern
      in
      let new_activity =
        Travelling
          {
            destination;
            path =
              Some
                ( destination_coords,
                  Tavern.find_path person.location destination_coords tavern );
          }
      in
      handle_travel { person with current_activity = new_activity } tavern
  | Travelling { destination; path = Some (dest_coord, coord :: path) } ->
      log `debug
        (Printf.sprintf "handling %s's travel: non-empty path present"
           person.name);
      if
        Tavern.is_occupied (Futil.Int_tuple_map.find dest_coord tavern)
        || Tavern.is_occupied (Futil.Int_tuple_map.find coord tavern)
      then
        ( {
            person with
            current_activity = Travelling { destination; path = None };
          },
          tavern )
      else
        (*
      match Futil.Int_tuple_map.find dest_coord tavern with
      | Floor (None, _) | Grass { occupant = None; _ } ->
          *)
        let updated_tavern = Tavern.move_person person coord tavern in
        ( {
            person with
            location = coord;
            current_activity =
              Travelling { destination; path = Some (dest_coord, path) };
          },
          updated_tavern )
        (*
      | _ ->
          let destination_coords =
            match destination with
            | "floor" -> Tavern.random_free_floor tavern
            | "bar" -> Tavern.random_stool tavern
            | d ->
                log `fatal ("impossible path in handle_travel: " ^ d);
                Tavern.random_free_floor tavern
          in
          handle_travel
            {
              person with
              current_activity =
                Travelling
                  {
                    destination;
                    path =
                      Some
                        ( destination_coords,
                          Tavern.find_path person.location destination_coords
                            tavern );
                  };
            }
            tavern)
        *)
  | Travelling { destination; path = Some (_, []) } ->
      log `debug
        (Printf.sprintf "handling %s's travel: empty path present" person.name);
      ( {
          person with
          current_activity = Travelling { destination; path = None };
        },
        tavern )
  | _ -> (person, tavern)

let rec manage_people people acc tavern : unit =
  match people with
  | person :: remaining_people ->
      log `info (Person.message_string_of_person person);
      Unix.sleepf 0.05;
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
      let updated_person, updated_tavern =
        handle_travel updated_person tavern
      in
      manage_people remaining_people (updated_person :: acc) updated_tavern
  | [] -> manage_people (List.rev acc) [] tavern

let run_manager () : unit =
  let people, tavern = initial_setup 5 in
  log `debug "starting manager...";
  if List.length people = 0 then log `fatal "Population cannot be 0"
  else manage_people people [] tavern
