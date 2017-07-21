open Blacs

type color = Black | White

let marks = Hashtbl.create 13

let rec eval_formula formulas lcell =
  let open LocatedCell in
  let open Cell        in
  let def    = lcell.cell.definition in
  let coords = lcell.coords          in
  if is_evaluated coords then
    ()
  else if is_being_evaluated coords then
    error coords def 
  else (
    mark_cell_as_being_evaluated coords;
    List.iter (eval_formula formulas) (dependencies formulas def);
    assign_cell coords (eval_formula_definition def)
  )
  
and dependencies formulas f =
  let open Coordinates in
  match f  with
    Definition.({ty=TyCount; data=Some [c;r;width;length;_] }) ->
    let inf,sup = coords c r, coords (c+length-1) (r+width-1)  in
    let range LocatedCell.({coords=c}) = inf <= c &&  c <= sup in
    List.filter range formulas
  | _ ->  []


and count i l =
  let open Cell        in
  let open Value       in
  let open LocatedCell in
  let x = ref 0 in
  let aux = function
      {cell = {value = Some {ty = TyInt; data= Some[i']}}} ->
      if i = i' then incr x;
    | _ -> ()
  in
  List.iter aux l;
  !x
           
and eval_formula_definition f =
  match f with
  Definition.({ ty = TyCount; data = Some [c; r; length; width; v] }) ->
  let cells = read Nativeint.zero (Coordinates.coords c r) length width in
  let i = count v cells in
  Cell.cell ~v:i f
  | _ -> Cell.cell f

and error coord f =
  let c =
    Cell.({definition=f; value=Some Value.null_int}) in
  assign_cell coord c

and assign_cell coord cell =
  write Nativeint.zero coord 1 1 [cell];
  mark_cell_as_evaluated coord

and mark_cell_as_being_evaluated coord =
  Hashtbl.add marks coord Black

and mark_cell_as_evaluated coord =
  Hashtbl.replace marks coord Black

and is_being_evaluated coord =
  try
    Black = Hashtbl.find marks coord
  with
  Not_found -> false
    
and is_evaluated coord =
  try
    White = Hashtbl.find marks coord
  with
  Not_found -> false

let init () =
  let cell  = Cell.cell        in
  let int   = Definition.int   in
  let count = Definition.count in
  let l =
    [cell ~v:1 (int 1); cell ~v:2 (int 2);cell ~v:3 (int 3); cell (count 0 0 1 4 1);
     cell ~v:1 (int 1); cell ~v:2 (int 2);cell ~v:3 (int 3); cell (count 0 1 1 4 2);
     cell ~v:1 (int 1); cell ~v:2 (int 2);cell ~v:3 (int 3); cell (count 0 2 1 4 3);
     cell ~v:1 (int 1); cell ~v:2 (int 2);cell ~v:3 (int 3); cell (count 0 3 1 4 4);
    ] in
  write_sheet Nativeint.zero 4 4 l

let main () =
  init ();
  let time         = get_time ()                     in
  let length,width = get_size ()                     in
  let formulas = read_formulas time length width     in
  List.iter (eval_formula formulas) formulas 

let () = main ()
