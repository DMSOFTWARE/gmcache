open Types

let usage () =
  print_endline "Usage: gmcache ([test <name>] | [<lat> <lon> <level>])"

let test () =
  match Sys.argv.(2) with
    | "geonames" -> Geonames.read ()
    | _ -> usage ()

let _ =
  let len = Array.length Sys.argv in
  if len = 1 || len = 4 then
    let lat = if len > 1 then float_of_string Sys.argv.(1) else 55.594255 in
    let lon = if len > 2 then float_of_string Sys.argv.(2) else 37.441406 in
    let level = if len > 3 then int_of_string Sys.argv.(3) else 3 in
    (try
      let ues =
	new Uq_gtk.gtk_event_system () in
      let cache =
	new Cache.block ues (Config.get_param "storage") in
      Geonames.read ();
      View.main ues ~lat ~lon ~level cache
    with
	exn -> usage (); raise exn)
  else
    if len = 2 then
      usage ()
    else
      test ()


