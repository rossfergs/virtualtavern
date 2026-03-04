type t =
  | Seeking_Conversation of { target : string option; topic : string }
  | In_Conversation of { partner : string; topic : string; length : int }

let message_string_of_conversation conv_info name thirst : string =
  match conv_info with
  | None -> Printf.sprintf "%s is socialising, thirst is %d" name thirst
  | Some (In_Conversation ci) ->
      Printf.sprintf "%s is talking to %s about %s (length = %d)" name
        ci.partner ci.topic ci.length
  | Some (Seeking_Conversation { target = Some target_name; topic }) ->
      Printf.sprintf "%s is looking to talk to %s about %s" name target_name
        topic
  | Some (Seeking_Conversation { target = None; topic }) ->
      Printf.sprintf "%s is looking to someone about %s (ERROR ?)" name topic
