open Yojson.Safe

let args    = Sys.argv

module BHost =
struct
  let host = Array.get args 1
  (* let host = "http://localhost:8080" *)
end

(* let sheet = "noname" *)

(* let tag   = "alice" *)

(* let default = "bob" *)
  
let sheet   = Array.get args 2

let tag     = Array.get args 3

let default = Array.get args 4

module BPervasives = BlacsPervasives.Make(BHost)

let get_size () =
  let open Dimensions in
  match BPervasives.io_size sheet with
    Result.Ok { length=l ; width=w }  -> l,w
  | Result.Error e -> failwith e

let get_time () =
  match BPervasives.clock_time sheet with
    Result.Ok  t -> t
  | Result.Error e -> failwith e

let get_hash hash =
  match BPervasives.io_hash sheet hash with
    Result.Ok lvl -> lvl
  | Result.Error e -> failwith e

let read_and_hash rrq =
  let open ReadPromise in
   match BPervasives.io_read sheet rrq with
    Result.Ok rp ->
    (if rp.date > (Unix.gettimeofday ())
     then
       Unix.sleepf (rp.date -. (Unix.gettimeofday ()) +. 0.005)
     else ());
    get_hash rp.hash
  | Result.Error e -> failwith e

let read_sheet time length width =
  let open ReadRequest in
  let rrq = read_request ~tag:tag ~time:time
      ~origin:(Coordinates.coords 0 0) ~length:length
      ~width:width ~filter_formulas:false ~default:(Some default) in
  read_and_hash rrq

let read_formulas time width length =
  let open ReadRequest in
  let rrq = read_request ~tag:tag ~time:time
      ~origin:(Coordinates.coords 0 0) ~length:length
      ~width:width ~filter_formulas:true ~default:(Some default) in
  read_and_hash rrq

let read time origin length width =
  let open ReadRequest in
  let rrq = read_request ~tag:tag ~time:time
      ~origin:origin ~length:length
      ~width:width ~filter_formulas:false ~default:(Some default) in
  read_and_hash rrq

let write_sheet time length width cells =
  let wrq = WriteRequest.write_request ~tag:tag
      ~time:time ~origin:(Coordinates.coords 0 0)
      ~length:length ~width:width ~cells:cells  in
  BPervasives.io_write sheet wrq

let write time origin length width cells =
  let wrq = WriteRequest.write_request ~tag:tag
      ~time:time ~origin:origin
      ~length:length ~width:width ~cells:cells in
  BPervasives.io_write sheet wrq

  
