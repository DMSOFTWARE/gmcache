(* resources.ml *)

open Types

(* Tracks *)

let list_of_directory dir =
  let dh = Unix.opendir dir in
  let acc = ref [] in
  try
    while true do
      let s = Unix.readdir dh in
      if s <> "." && s <> ".." then
	acc := !acc @ [s];      
    done; []
  with End_of_file -> !acc

let track_list () =
  let dir = 
    Filename.concat
      (Config.get_param "resource-dir")
      "tracks"
  in List.filter 
       (Pcre.pmatch ~pat:"\\.kml$")
       (list_of_directory dir)  
  
let load_tracks ?(hd=0) ?(tl=0) file =
  let tracks =
    print_string "loading tracks...";
    flush stdout;
    Kml.load_gps_tracks ~hd ~tl file;
  in print_endline "ok";
  flush stdout; tracks

let search_trackpoints ~area tracks =
  print_endline "search trackpoints";
  let (lb,rb,tb,bb) = area in
  List.map
    (fun track -> 
      List.filter (fun (lat,lon) -> 
	lat > lb && lat < rb && lon < tb && lon > bb)
      track)
    tracks
  
    
