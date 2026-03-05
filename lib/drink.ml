type size = Shot | Small | Medium | Large
type t = { name : string; size : size; potency : int }

let random_size () =
  match Random.int 4 with
  | 0 -> Shot
  | 1 -> Small
  | 2 -> Medium
  | 3 -> Large
  | _ ->
      Futil.fatal "Unreachable path in Drink.random_size";
      raise (Failure "See Logs For Details")

let random_name () =
  let adj_list =
    Futil.read_lines "lib/resources/words/adjectives/adjectives.txt"
  in
  let noun_list = Futil.read_lines "lib/resources/words/nouns/nouns.txt" in
  let a = Random.int (List.length adj_list) in
  let n = Random.int (List.length noun_list) in
  String.trim (List.nth adj_list a) ^ " " ^ String.trim (List.nth noun_list n)

let random_drink () =
  { name = random_name (); size = random_size (); potency = Random.int 100 }

let drinks = List.init 5 (fun _ -> random_drink ())
let select_drink () = List.nth drinks (Random.int (List.length drinks))
