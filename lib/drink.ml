type potency = High | Medium | Low | None
type t = { name : string; size : int; potency : potency }

let[@warning "-6"] random_drink () =
  let choice = Random.int_in_range ~min:0 ~max:3 in
  match choice with
  | 0 -> { name = "water"; size = 3; potency = None }
  | 1 -> { name = "beer"; size = 3; potency = Low }
  | 2 -> { name = "wine"; size = 2; potency = Medium }
  | 3 -> { name = "shot"; size = 1; potency = High }
  | _ -> { name = "unknown drink (ERROR)"; size = 1; potency = None }

let verb_drink (drink : t) : string =
  match drink.name with
  | "beer" | "shot" -> "a " ^ drink.name
  | "wine" | "water" -> "some " ^ drink.name
  | _ -> "an unknown drink (ERROR)"
