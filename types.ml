(* types.ml *)

type level = int       (* 1 - minimal zoom, 20 - maximal zoom *)
type latitude = float  (* latitude/широта   *)
type longitude = float (* longitude/долгота *)
type coordinate = latitude * longitude
type track = coordinate list

let pi = 3.14159265358979323846264338327950288419716939937510

let block_size = 256

let num_tiles level =
  int_of_float
    (2.0 ** (float_of_int (level - 1)))

let bitmap_size level =
  block_size * (num_tiles level)

let bitmap_origo level =
  (bitmap_size level) / 2

exception Invalid_latitude of float
exception Invalid_longitude of float

let check_latitude n =
  if n > 85.0 || n < -85.0 then
    raise (Invalid_latitude n)

let check_longitude n =
  if n > 180.0 || n < -180.0 then
    raise (Invalid_longitude n)

let check_coordinate l =
  check_latitude (fst l);
  check_longitude (snd l)

let pixels_per_lon_degree level =
  (float_of_int (bitmap_size level)) /. 360.0

let pixels_per_lon_radian level =
  (float_of_int (bitmap_size level)) /. (2.0*.pi)

let radian_of_degree n =
  n *. pi /. 180.0

let bitmap_of_geo coord level =
  check_coordinate coord;
  
  let lat = fst coord in
  let lon = snd coord in
  let z =
    sin (radian_of_degree lat) in
  let x =
    floor ((float_of_int (bitmap_origo level)) +. lon *. (pixels_per_lon_degree level)) in
  let y =
    floor ((float_of_int (bitmap_origo level)) -. (0.5 *. log ((1.0 +. z) /. (1.0 -. z)) *. (pixels_per_lon_radian level))) in
  x,y

let geo_of_bitmap coord level =
  let x = fst coord in
  let y = snd coord in
  let z =
    ((float_of_int (bitmap_origo level)) -. y) /. (pixels_per_lon_radian level) in
  let lat =
    ((2.0 *. atan (exp z)) -. (pi /. 2.0)) *. (180.0 /. pi) in
  let lon = 
    (x -. (float_of_int (bitmap_origo level))) /. (pixels_per_lon_degree level) in  
  lat,lon

let geo_null level =
  bitmap_origo level

let block_name_with_bitmap_point bmp level =
  let (x,y) = bmp in 
  let nx = ref (int_of_float (floor (x /. (float_of_int block_size)))) in
  let ny = ref (int_of_float (floor (y /. (float_of_int block_size)))) in
  let d = ref (num_tiles level) in
  let res = Buffer.create 8 in
  let add = Buffer.add_char res in
  
  if !nx < 0 || (!d - 1) < !nx then
    begin
      nx := !nx mod !d;
      if !nx < 0 then 
	nx := !nx + !d
    end;
  
  add 't';
  
  for i=2 to level do
    d := !d / 2;
    (* Printf.printf "d(%d), nx(%d), ny(%d)\n" !d !nx !ny; *)
    if !ny < !d then
      begin
	if !nx < !d then
	  add 'q'
	else
	  (add 'r'; nx := !nx - !d)
      end
    else
      begin
	if !nx < !d then
	  add 't'
	else
	  (add 's'; nx := !nx - !d);
	ny := !ny - !d
      end
  done;

  Buffer.contents res

let block_name geo level =
  block_name_with_bitmap_point
    (bitmap_of_geo geo level) level
  
