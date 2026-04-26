exception Not_In_Conversation

let log = Futil.Logging.log ~tag:__MODULE__ ~min_level:`info

type activity =
  | Drinking of { drink : Drink.t; remaining_turns : int }
  | Socialising of Conversation.t
  | Travelling of {
      destination : string;
      path : ((int * int) * (int * int) list) option;
    }
  | None

type t = {
  name : string;
  current_activity : activity;
  location : int * int;
  thirst : int;
}

let make_person name location =
  { name; current_activity = None; location; thirst = 50 }

let handle_socialising (person : t) (conv_info : Conversation.t) : t =
  match conv_info with
  | Seeking_Conversation _ -> person
  | No_Conversation ->
      let new_thirst = person.thirst - Random.int 11 in
      if new_thirst <= 0 then
        {
          person with
          current_activity = Travelling { destination = "bar"; path = None };
          thirst = 0;
        }
      else { person with thirst = new_thirst }
  | In_Conversation { partner; topic; length } -> (
      let new_thirst = person.thirst - Random.int 7 in
      match (new_thirst, length) with
      | _, _ when new_thirst <= 0 ->
          {
            person with
            thirst = 0;
            current_activity = Travelling { destination = "bar"; path = None };
          }
      | _, 10 -> { person with current_activity = Socialising No_Conversation }
      | _, _ when Random.int (11 - length) = 0 ->
          {
            person with
            current_activity = Socialising No_Conversation;
            thirst = new_thirst;
          }
      | _, _ ->
          {
            person with
            current_activity =
              Socialising
                (In_Conversation { partner; topic; length = length + 1 });
            thirst = new_thirst;
          })

let get_pace drink =
  let open Drink in
  match drink.size with
  | Shot -> 1
  | Small -> ( match Random.int 3 with 0 -> 5 | 1 -> 3 | _ -> 1)
  | Medium -> ( match Random.int 3 with 0 -> 5 | 1 -> 3 | _ -> 1)
  | Large -> ( match Random.int 3 with 0 -> 5 | 1 -> 3 | _ -> 1)

let update_activity (person : t) =
  match person.current_activity with
  | Travelling { destination; path = Some (c, _) } when person.location = c ->
      log `debug "reached destination";
      let new_activity =
        match destination with
        | "bar" ->
            Drinking { drink = Drink.select_drink (); remaining_turns = 3 }
        | "floor" -> Socialising No_Conversation
        | _ -> None
      in
      { person with current_activity = new_activity }
  | Travelling _ -> person
  | Drinking { drink = _; remaining_turns = 0 } ->
      let new_thirst = if person.thirst > 100 then 100 else person.thirst in
      let another_drink_chance = Random.int 101 in
      if another_drink_chance <= person.thirst then
        {
          person with
          current_activity = Travelling { destination = "floor"; path = None };
          thirst = new_thirst;
        }
      else
        let chosen_drink = Drink.select_drink () in
        {
          person with
          current_activity =
            Drinking
              { drink = chosen_drink; remaining_turns = get_pace chosen_drink };
          thirst = new_thirst;
        }
  | Drinking drink ->
      let new_thirst = person.thirst + Random.int_in_range ~min:10 ~max:30 in
      {
        person with
        current_activity =
          Drinking { drink with remaining_turns = drink.remaining_turns - 1 };
        thirst = new_thirst;
      }
  | Socialising conv_info -> handle_socialising person conv_info
  | None ->
      if Random.bool () then
        {
          person with
          current_activity = Travelling { destination = "bar"; path = None };
        }
      else
        {
          person with
          current_activity = Travelling { destination = "floor"; path = None };
        }

(*let string_of_person person = *)
(*  let open Futil.Format in *)
(*  match person with *)
(*  | { current_activity = Socialising (Seeking_Conversation)} -> *)
(*  | _ -> "@" *)

let message_string_of_person (person : t) : string =
  match person.current_activity with
  | Travelling { destination; path = Some (_, (x, y) :: _) } ->
      Printf.sprintf "%s is walking to the %s, coordinate (%d, %d)" person.name
        destination x y
  | Travelling { destination; path = Some (_, []) } ->
      Printf.sprintf
        "%s is walking to the %s, but doesnt have a plan on how to get there"
        person.name destination
  | Travelling { destination; path = None } ->
      Printf.sprintf "%s is walking to the %s, they dont know where to go!"
        person.name destination
  | Drinking { drink; remaining_turns } ->
      Printf.sprintf "%s is drinking %s, thirst is %d, remaining turns is %d"
        person.name ("a" ^ drink.name) person.thirst remaining_turns
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
      current_activity = Socialising (In_Conversation { partner = name; _ });
      _;
    }
  | {
      current_activity =
        Socialising (Seeking_Conversation { target = Some name; _ });
      _;
    }
    when p1.name = name ->
      true
  | _ -> false

let get_conversation_topic = function
  | { current_activity = Socialising (Seeking_Conversation { topic; _ }); _ } ->
      topic
  | { current_activity = Socialising (In_Conversation { topic; _ }); _ } ->
      topic
  | _ -> raise Not_In_Conversation
