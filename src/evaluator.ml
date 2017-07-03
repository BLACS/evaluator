module HTC = Hashtbl.Make(Coordinates)
    
type color = Black | Grey | White


type readRQ           = {tag          : string        ;
                         time         : Nativeint.t   ;
                         origin       : Coordinates.t ;
                         width        : int           ;
                         length       : int           ;
                         default      : string option } [@@deriving yojson]

type writeRQ          = {tag          : string        ;
                         time         : Nativeint.t   ;
                         origin       : Coordinates.t ;
                         length       : int           ;
                         width        : int           ;
                         values       : Value.t list  } [@@deriving yojson]

type readPromise      =  {date        : float         ;
                          hash        : string        } [@@deriving yojson]

type locatedValue     = {coords       : Coordinates.t ;
                         value        : Value.t       } [@@deriving yojson]

type locatedValueList =  locatedValue list  [@@deriving yojson]

let bind x f = match x with
    Some v -> f v
  | None   -> None

let ( >>= )  = bind

let return   = fun x -> Some x

let readrq tag time origin width length default =
  {tag           = tag     ;
   time          = time    ;
   origin        = origin  ;
   width         = width   ;
   length        = length  ;
   default       = default }

let writerq tag time origin width length values =
  {tag           = tag     ;
   time          = time    ;
   origin        = origin  ;
   width         = width   ;
   length        = length  ;
   values        =  values }

let filterTable table predicate =
  let filter c v acc =
    if  predicate c v
    then c::acc
    else acc
  in
  return (HTC.fold filter table [])

let isCount c v = Value.(
  match v with
    _, { ty = TyCount; data = Some _ } -> true
  | _                                  -> false)

let filterCountFormulas values =
  filterTable values isCount

let inRange inf sup c v =
  Coordinates.(lteq inf c && lteq c sup)

let dependencies f formulas =
  let open Value                                 in
  let open Coordinates                           in
  match f with
    {ty=TyCount; data=Some([r1;c1;width;length;_])} ->
    let inf,sup = coords r1 c1, coords (r1+width-1) (c1+length-1) in
    let range c = lteq inf c && lteq c sup in
    return (List.filter range formulas)
  | _ ->  None

let findSchedule values formulas coord =
  let rec aux output vertex =
    let (color,formula) = HTC.find values vertex     in
    match color,dependencies formula formulas with
      Black,_          -> output
    | Grey,_           ->
      HTC.replace values vertex (White,Value.nullInt);
      None
    | White,None       ->
      HTC.replace values vertex (Black,formula);
      return ([vertex])
    | White,Some(deps) ->
      HTC.replace values vertex (Grey,formula)       ;
      List.fold_left aux output deps                 >>= fun output -> 
      HTC.replace values vertex (Black,formula)      ;
      return (vertex::output)    
  in
  try
    aux (return []) coord
  with
    Not_found -> None

let eval c values =
  let open Value                                   in
  let open Coordinates                             in
  let f = HTC.find values c                        in
  match f with
    Grey,_ -> None
  | _,{ ty=TyCount ; data=Some ([r1;c1;width;length;i]) } ->
    let inf,sup = (coords r1 c1),(coords (r1+width-1) (c1+length-1))    in
    let inRange = inRange inf sup                  in
    let predicate c v = match v with
        _,{ ty=TyInt ; data = Some [i'] }          ->
        inRange c v && i=i' 
      | _ -> false
    in
    filterTable values predicate                   >>= fun l ->
    return (Value.int (List.length l))
  | _,_    -> None

let valuesTable vl vt =
  List.iter (fun {coords=c; value=v} -> HTC.add vt c (White,v)) vl;
  vt

let evalSchedule vt s  =
  let s = List.rev s in 
  List.iter
    (fun x ->
       match eval x vt with
         Some v -> HTC.replace vt x (White,v)
       | None   -> ()) s

let url        = "http://localhost:8080" 

let sheet      = "test"

let tag        = "alice"

let default    = "bob"

let httpHeader = ["accept: application/json"; "content-type: application/json"]

let httpConnection url writeFunction httpHeader =
  let open Curl in
  let connection = init () in
  set_url connection url;
  set_writefunction connection writeFunction;
  set_httpheader connection httpHeader;
  connection

let postConnection url writeFunction httpHeader httpBody =
  let open Curl in
  let connection = httpConnection url writeFunction httpHeader      in
  set_post connection true;
  set_postfields connection httpBody;
  connection


let getConnection url writeFunction httpHeader getParams =
  let open Curl in
  let urlParams =
    (function
      None -> url
    | Some params -> Printf.sprintf "%s/%s" url params) getParams    in
  let connection = httpConnection urlParams writeFunction httpHeader in
  connection

let performConnection connection =
  let open Curl in
  perform connection;
  get_responsecode connection
      
let getSize () = 4,11


let getTime () =
  let timeSheet      = return  (Printf.sprintf "time/%s" sheet)      in
  let buffer         = Buffer.create 128                             in
  let write          = fun data ->
    Buffer.add_string buffer data;
    String.length data                                               in
  let connection     = getConnection url write httpHeader timeSheet  in
  let _              = performConnection connection                  in
  Curl.cleanup connection;
  let bufferContents = Buffer.contents buffer                        in
  Nativeint.of_string bufferContents

let getHash hash buffSize =
  let sheetHash  = return (Printf.sprintf "%s/%s" sheet hash)        in
  let buffer         = Buffer.create buffSize                        in
  let write          = fun data ->
    Buffer.add_string buffer data;
    String.length data                                               in
  let connection     = getConnection url write httpHeader sheetHash  in
  let _              = performConnection connection                  in
  Curl.cleanup connection;
  let bufferContents = Buffer.contents buffer                        in
  let yojson         = Yojson.Safe.from_string bufferContents        in
  match locatedValueList_of_yojson yojson with
    Result.Ok(vl) -> vl
  | _ -> assert false

let readSheet time width length =
  let buffer = Buffer.create 512                                    in
  let write data =
    Buffer.add_string buffer data;
    String.length data                                              in
  let rrq            = readrq tag time
      (Coordinates.coords 0 0) width length (return default)        in
  let httpBody       =
    readRQ_to_yojson rrq |> Yojson.Safe.to_string                   in
  let url = Printf.sprintf "%s/read/%s" url sheet                   in
  let connection     = postConnection url write httpHeader httpBody in
  let _ = performConnection connection                           in
  Curl.cleanup connection;
  let bufferContents = Buffer.contents buffer                       in
  let yojson         = Yojson.Safe.from_string bufferContents       in
  let rp             = match readPromise_of_yojson yojson with
      Result.Ok(rp) -> rp
    | _ -> assert false in
  (if rp.date > (Unix.gettimeofday ())
   then
     Unix.sleepf (rp.date -. (Unix.gettimeofday ()) +. 0.5)
   else ());
  getHash rp.hash 2048

let writeSheet time width length values =
  let buffer = Buffer.create 2                                       in
  let write data =
    Buffer.add_string buffer data;
    String.length data                                               in
  let wrq            = writerq tag time
      (Coordinates.coords 0 0) width length values                   in
  let httpBody       =
    writeRQ_to_yojson wrq |> Yojson.Safe.to_string                   in
  let url = Printf.sprintf "%s/write/%s" url sheet                   in
  let connection     = postConnection url write httpHeader httpBody  in
  let _ = performConnection connection                               in
  Curl.cleanup connection

let values  () =
  let width,length = getSize () in
  let time         = getTime () in
  return (readSheet time width length)

let writeValues values =
  let width,length = getSize () in
  let time         = getTime () in
  writeSheet time width length values


let compareLocVal (c1,_) (c2,_) =
  Coordinates.compare c1 c2
  

            
let main () =
  let open Value in
  let l = [int 1; int 2;int 3; int 4; int 5;int 6; int 7;int 8; int 9;int 10; count 1 0 3 11 1;
           int 1; int 2;int 3; int 4; int 5;int 6; int 7;int 8; int 9;int 10; count 2 0 1 11 2;
           int 1; int 2;int 3; int 4; int 5;int 6; int 7;int 8; int 9;int 10; count 3 0 1 11 3;
           int 1; int 2;int 3; int 4; int 5;int 6; int 7;int 8; int 9;int 10; count 3 0 1 10 4] in
  writeValues l;
  let valSchedOpt =
    values ()                                                 >>= fun vals ->
    return (valuesTable vals (HTC.create (List.length vals))) >>= fun vt   ->
    filterCountFormulas vt                                    >>= fun f    ->
    return ( vt, List.map (findSchedule vt f) f)                                   in
  (match valSchedOpt with
     Some (vt,s) ->
     List.iter (function None   -> ()| Some l -> (evalSchedule vt) l) s;
     let values = HTC.fold (fun c (col,v) acc -> (c,v)::acc) vt [] in
     let values = List.sort compareLocVal values  in
     List.iter (fun (c,v) -> print_endline (
         Value.string_of_value v ^ " @ " ^ Coordinates.to_string c);flush_all ()) values;
     let values = snd (List.split values) in
     writeValues values
   | None -> ())

let () = main ()
