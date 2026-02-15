type location = Bar | Floor

type activity =
  | Drinking of Drink.t
  | Socialising
  | Travelling of (location * int)
  | None

type t = { name : string; current_activity : activity; thirst : int }

let make_person name = { name; current_activity = None; thirst = 50 }

let update_activity (person : t) =
  match person.current_activity with
  | Travelling (Floor, 0) -> { person with current_activity = Socialising }
  | Travelling (Bar, 0) ->
      { person with current_activity = Drinking (Drink.random_drink ()) }
  | Travelling (location, distance) ->
      { person with current_activity = Travelling (location, distance - 1) }
  | Drinking { name = _; size = 0; potency = _ } ->
      let another_drink_chance = Random.int_in_range ~min:0 ~max:100 in
      let new_thirst = if person.thirst > 100 then 100 else person.thirst in
      if another_drink_chance <= person.thirst then
        {
          person with
          current_activity = Travelling (Floor, 3);
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
  | Socialising ->
      let new_thirst = person.thirst - Random.int_in_range ~min:1 ~max:10 in
      if new_thirst <= 0 then
        { person with current_activity = Travelling (Bar, 3); thirst = 0 }
      else { person with thirst = new_thirst }
  | None ->
      let decision = Random.int_in_range ~min:0 ~max:1 in
      if decision = 0 then
        { person with current_activity = Travelling (Bar, 6) }
      else { person with current_activity = Travelling (Floor, 6) }

let message_string_of_person (person : t) : string =
  match person.current_activity with
  | Travelling (Bar, distance) ->
      Printf.sprintf "%s is walking to the %s, distance remaining is %d"
        person.name "bar" distance
  | Travelling (Floor, distance) ->
      Printf.sprintf "%s is walking to the %s, distance remaining is %d"
        person.name "floor" distance
  | Drinking drink ->
      Printf.sprintf "%s is drinking %s, thirst is %d" person.name
        (Drink.verb_drink drink) person.thirst
  | Socialising ->
      Printf.sprintf "%s is socialising, thirst is %d" person.name person.thirst
  | None -> Printf.sprintf "%s is doing nothing" person.name
