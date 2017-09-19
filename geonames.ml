(* geonames.ml *)

open Types
open Printf

let locations = ref []
let cachefile = 
  Filename.concat 
    (Config.get_param "resource-dir")
    "locations/cache"

(* 451747 Zyabrikovo Zyabrikovo 56.8463889 34.7072222 P PPL RU 77 0189 Europe/Moscow 1998-05-01 *)

let read_directory dir =
  let dh = Unix.opendir dir in
  let rec read acc = 
    try
      let s = Unix.readdir dh in
      if s <> "." && s <> ".." then
	read (acc @ [s])
      else read acc
    with End_of_file ->
      Unix.closedir dh; acc
  in read []

let is_float s =
  try
    ignore (float_of_string s); true
  with _ -> false

let rec parse geo names = function
  | [] -> geo,names
  | hd::tl ->
      if is_float hd then
	parse (float_of_string hd, float_of_string (List.hd tl)) names []
      else
	if hd = "" then
	  parse geo names tl
	else
	  parse geo (hd::names) tl

let read () =
  print_string "loading geonames..."; flush stdout;
  if Sys.file_exists cachefile then
    let ch = open_in cachefile in
    locations := (Marshal.from_channel ch);
    close_in ch
  else 
    begin
      let input_locs ch =
	let s = input_line ch in
	let rex = Pcre.regexp "\t" in
	let l = 
	  List.tl (Pcre.split ~rex s) in
	locations := (parse (0.0,0.0) [] l)::!locations
      in
      let dir = 
	Filename.concat 
	  (Config.get_param "resource-dir") "locations" in
      List.iter
	(fun name ->
	  if Pcre.pmatch ~pat:"\\.txt$" name then
	    begin
	      let file = Filename.concat dir name in
	      let ch = open_in file in
	      try
		while true do
		  input_locs ch
		done
	      with End_of_file ->
		close_in ch
	    end)
	(read_directory dir);
      let ch = open_out cachefile in
      Marshal.to_channel ch !locations [];
      close_out ch
    end;      
  print_endline "ok"

let search ~area =
  let (lb,rb,tb,bb) = area in
  List.filter
    (fun ((lat,lon),_) ->
      lat > lb && lat < rb && 
      lon < tb && lon > bb)
    !locations
