open Minttea

type model = { tavern : Tavern.t; people : Person.t list }
let updates = ref 0
let initial_model =
  let people, tavern = Manager.initial_setup 5 in
  { tavern; people }

let init _model = Command.Noop
let fps = Leaves.Fps.of_int 10
let paused = ref false

let update event { people; tavern } =
  if Leaves.Fps.tick fps = `skip then ({ people; tavern }, Command.Noop)
  else
    match event with
    | _ ->
        let updated_people, updated_tavern =
          Manager.manage_people people [] tavern
        in
        updates := !updates + 1;
        ({ people = updated_people; tavern = updated_tavern }, Command.Noop)

let view { tavern; _ } =
  (* let open Futil.Logging in *)
  let open Futil.Windower in
  let tav_top_x, tav_top_y = (1, 1) in
  (* let log_top_x, log_top_y = (tav_top_x + Tavern.max_x + 1, 1) in *)
  let tavern_window =
    make_window (Tavern.string_of_tavern tavern) tav_top_x tav_top_y
  in
  let update_window = make_window (string_of_int !updates) 1 1 in
  (* let _log_window =
    make_window
      (List.fold_left (fun acc l -> acc ^ l ^ "\n") "" !previous_logs)
      log_top_x log_top_y
  in *)
  let test_window_1 =
    make_window "This is a test window\nIs it working?"
      (tavern_window.x + tavern_window.width + 12)
      1
  in
  let test_window_2 =
    make_window ~bordered:true "This is a test window\nIs it working?\n\n\n\n\n\n\n\n\n\n\n\n"
      (tavern_window.x + tavern_window.width - 4)
      12
  in
  empty |> add update_window |> add @@  test_window_2
  |> add tavern_window |> add test_window_1 |> render_screen

let app = Minttea.app ~init ~update ~view ()
let start () = Minttea.start app ~initial_model
