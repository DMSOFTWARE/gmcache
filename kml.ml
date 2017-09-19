(* kml.ml *)

open Types

open Pxp_types
open Pxp_document
open Pxp_dtd_parser
open Pxp_tree_parser

let head n l =
  let rec make acc x = function
      [] -> acc
    | hd::tl ->
	if x < n then
	  make (hd::acc) (succ x) tl
	else
	  acc
  in List.rev (make [] 0 l)

let tail n l =
  let z = List.length l in
  let rec make acc x = function
      [] -> acc
    | hd::tl ->
	if z - x <= n then
	  make (hd::acc) (succ x) tl
	else
	  make acc (succ x) tl	  
  in List.rev (make [] 0 l)

let open_document file =
  let pxp_config =
    { default_config with 
      encoding = `Enc_utf8;
      drop_ignorable_whitespace = false;
    } 
  in parse_wfdocument_entity pxp_config
       (from_file file) default_spec

let parse_coordinates ~hd ~tl s =
  let rex = Pcre.regexp "\\n+" in
  let spl = Pcre.regexp "," in
  let coords =
    List.map
      (fun s2 ->
	let l = Pcre.split ~rex:spl s2 in
	float_of_string (List.hd (List.tl l)),
	float_of_string (List.hd l))
      (List.filter
	(Pcre.pmatch ~rex:spl)
	(Pcre.split ~rex s))
  in match hd,tl with
    | 0,0 -> coords
    | _,0 -> head hd coords
    | 0,_ -> tail tl coords
    | _,_ -> head hd (tail tl coords)

let find_coordinates ~hd ~tl node =
  List.map
    (fun n -> parse_coordinates ~hd ~tl n # data)
    (find_all ~deeply:true
      (fun n ->
	match n # node_type with
	  | T_element "coordinates" ->  true
	  | _ -> false)
      node)

let load_gps_tracks ?(hd=0) ?(tl=0) file =
  let doc =
    open_document file in
  find_coordinates ~hd ~tl doc#root




