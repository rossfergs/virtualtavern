exception Not_A_Conversation

type location = Bar | Floor

type activity =
  | Drinking of Drink.t
  | Socialising of Conversation.t option
  | Travelling of { destination : location; distance : int }
  | None

type t = { name : string; current_activity : activity; thirst : int }

let make_person name = { name; current_activity = None; thirst = 50 }

let handle_socialising (person : t) (conv_info : Conversation.t option) : t =
  match conv_info with
  | None ->
      let new_thirst = person.thirst - Random.int_in_range ~min:1 ~max:10 in
      if new_thirst <= 0 then
        {
          person with
          current_activity = Travelling { destination = Bar; distance = 2 };
          thirst = 0;
        }
      else { person with thirst = new_thirst }
  | Some conv_info -> (
      match conv_info with
      | In_Conversation { partner; topic; length } ->
          let new_thirst = person.thirst - Random.int_in_range ~min:1 ~max:10 in
          if new_thirst <= 0 then
            {
              person with
              current_activity = Travelling { destination = Bar; distance = 2 };
              thirst = 0;
            }
          else if length = 10 then
            { person with current_activity = Socialising None }
          else
            let conv_end_chance =
              Random.int_in_range ~min:0 ~max:(10 - length)
            in
            if conv_end_chance = 0 then
              {
                person with
                current_activity = Socialising None;
                thirst = new_thirst;
              }
            else
              {
                person with
                current_activity =
                  Socialising
                    (Some
                       (In_Conversation { partner; topic; length = length + 1 }));
                thirst = new_thirst;
              }
      | Seeking_Conversation _ -> person)

let update_activity (person : t) =
  match person.current_activity with
  | Travelling { destination = Floor; distance = 0 } ->
      { person with current_activity = Socialising None }
  | Travelling { destination = Bar; distance = 0 } ->
      { person with current_activity = Drinking (Drink.select_drink ()) }
  | Travelling { destination; distance } ->
      {
        person with
        current_activity = Travelling { destination; distance = distance - 1 };
      }
  | Drinking { name = _; size = 0; potency = _ } ->
      let another_drink_chance = Random.int_in_range ~min:0 ~max:100 in
      let new_thirst = if person.thirst > 100 then 100 else person.thirst in
      if another_drink_chance <= person.thirst then
        {
          person with
          current_activity = Travelling { destination = Floor; distance = 2 };
          thirst = new_thirst;
        }
      else
        {
          person with
          current_activity = Drinking (Drink.random_drink ());
          thirst = new_thirst;
        }
  | Drinking drink ->
      let new_thirst = person.thirst + Random.int_in_range ~min:10 ~max:50 in
      {
        person with
        current_activity = Drinking { drink with size = drink.size - 1 };
        thirst = new_thirst;
      }
  | Socialising conv_info -> handle_socialising person conv_info
  | _ ->
      let decision = Random.int_in_range ~min:0 ~max:1 in
      if decision = 0 then
        {
          person with
          current_activity = Travelling { destination = Bar; distance = 3 };
        }
      else
        {
          person with
          current_activity = Travelling { destination = Floor; distance = 3 };
        }

let message_string_of_person (person : t) : string =
  match person.current_activity with
  | Travelling { destination = Bar; distance } ->
      Printf.sprintf "%s is walking to the %s, distance remaining is %d"
        person.name "bar" distance
  | Travelling { destination = Floor; distance } ->
      Printf.sprintf "%s is walking to the %s, distance remaining is %d"
        person.name "floor" distance
  | Drinking drink ->
      Printf.sprintf "%s is drinking %s, thirst is %d" person.name
        (Drink.verb_drink drink) person.thirst
  | Socialising conv_info ->
      Conversation.message_string_of_conversation conv_info person.name
        person.thirst
  | None -> Printf.sprintf "%s is doing nothing" person.name

let is_socialising = function
  | { current_activity = Socialising _; _ } -> true
  | _ -> false

let are_players_conversing p1 p2 =
  match p2 with
  | {
      current_activity =
        Socialising (Some (In_Conversation { partner = name; _ }));
      _;
    }
  | {
      current_activity =
        Socialising (Some (Seeking_Conversation { target = Some name; _ }));
      _;
    }
    when p1.name = name ->
      true
  | _ -> false

let get_conversation_topic = function
  | {
      current_activity = Socialising (Some (Seeking_Conversation { topic; _ }));
      _;
    } ->
      topic
  | { current_activity = Socialising (Some (In_Conversation { topic; _ })); _ }
    ->
      topic
  | _ -> raise Not_A_Conversation
