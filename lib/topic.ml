let generate_person_topic people = Futil.rand_from_list people

let verbs =
  Futil.read_lines "lib/resources/words/verbs/verbs.txt"
  |> List.map String.trim
  |> List.filter (fun s -> String.ends_with ~suffix:"ing" s)

let adverbs =
  Futil.read_lines "lib/resources/words/adverbs/adverbs.txt"
  |> List.map String.trim
  |> List.filter (fun s -> String.ends_with ~suffix:"ly" s)

let generate_other_topic () =
  let verb = Futil.rand_from_list verbs in
  let adverb = Futil.rand_from_list adverbs in
  adverb ^ " " ^ verb

let generate_topic people =
  match Random.int 2 with
  | 0 -> generate_person_topic people
  | 1 -> generate_other_topic ()
  | _ ->
      Futil.fatal "Unreachable path in Topic.generate_topic";
      raise (Failure "see logs for more details")
