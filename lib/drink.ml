type potency = int
type t = { name : string; size : int; potency : potency }

let random_name () =

        let adj_list = Futil.read_lines "lib/resources/words/adjectives/adjectives.txt" in
        let noun_list = Futil.read_lines "lib/resources/words/nouns/nouns.txt" in
        let a = Random.int (List.length adj_list) in
        let n = Random.int (List.length noun_list) in
        (String.trim (List.nth adj_list a)) ^ " " ^ (String.trim (List.nth noun_list n))
        


let [@warning "-6"] random_drink () =     
  { 
  name = random_name (); 
  size = Random.int(100); 
  potency = Random.int(100) 
  }

let drinks = List.init 5 (fun _ -> random_drink ())

let verb_drink (drink : t) : string =
  "a " ^ drink.name

let select_drink ()  = 
        List.nth drinks (Random.int (List.length drinks))   

