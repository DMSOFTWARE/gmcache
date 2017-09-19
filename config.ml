open Printf

exception Unknown_parameter of string

let list_of_channel ch =
  let rec read acc =
    try
      read (acc @ [input_line ch])
    with End_of_file -> acc
  in read []

let read_params () =
  let params = Hashtbl.create 32 in
  let home = Sys.getenv "HOME" in
  let file = Filename.concat home ".gmcache" in
  let filename =
    if Sys.file_exists file then
      Some file
    else
      if Sys.file_exists "/etc/gmcache"
      then Some "/etc/gmcache" else None
  in match filename with
    | None -> params
    | Some filename ->
	let rex = Pcre.regexp "^([^#\\s]+)\\s+(.*)\\s*$" in
	let ch = open_in filename in
	List.iter
	  (fun s ->
	    if Pcre.pmatch ~rex s then
	      let a =
		Pcre.extract ~rex s
	      in Hashtbl.replace params a.(1) a.(2))
	  (list_of_channel ch);
	close_in ch;
	params

let user_params = 
  read_params ()

let set_default s default =
  let value =
    try
      Hashtbl.find user_params s
    with Not_found -> default
  in Hashtbl.replace user_params s value

let get_param s =
  try 
    Hashtbl.find user_params s
  with Not_found -> raise (Unknown_parameter s)

let update_param name value =
  Hashtbl.replace user_params name value

let print_actual_params () =
  print_endline "=> configuration";
  Hashtbl.iter
    (fun k v ->
      print_endline (sprintf "%s %s" k v))
    user_params;
;;

set_default "debug" "false";;
set_default "verbose" "true";;
set_default "offline" "true";;
set_default "storage" "/home/maps/maps-level-10";;
set_default "gps-serial" "/dev/rfcomm0";;
set_default "gps-btaddr" "00:0D:B5:A0:04:87";;
set_default "gps-btface" "hci0";;
set_default "gps-required" "false";;
set_default "maps-version" "0";;
set_default "fetch-delay" "1.0";;
set_default "share-dir" "share";;
set_default "resource-dir" "/home/gmcache";;
set_default "user-agent" "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9b4) Gecko/2008030318 Firefox/3.0b4";;
set_default "cookie-khcookie" "fzwq2pVdwubEw01e4K9iF8LlXlWd2XaDcsAQbA";;
set_default "cookie-pref" "ID=64a9961894e6b254:TM=1212195826:LM=1212251704:GM=1:S=EQRDlWB0m3SbPsCg";;
set_default "cookie-nid"
  "11=WQ4URh65S9H7iWxuoVARD2SEzN-BFEYjAhQlgQb32wj_t4B-Lm3eBPm8owx588neDSrBKCm-6fae9HhGeGusaRuHTcHQE6aqEPr1DksbxlchNAC6jPw1Bm1W6W7fFGKE";;

if get_param "verbose" = "true" then
  print_actual_params ()

