open Brr

let all : Cell.t list ref = ref []
let id_counter = ref 0

let setup () =
  let delete_cell id =
    match List.find_opt (fun c -> Cell.id c = id) !all with
    | None -> ()
    | Some cell ->
      Cell.remove cell;
      all := List.filter (fun c -> Cell.id c <> id) !all
  in
  Jv.set Jv.global "xOcamlDeleteCell"
    (Jv.callback ~arity:1 (fun args ->
      delete_cell (Jv.to_int args.(0));
      Jv.undefined));
  let insert_after new_id after_id =
    match List.find_opt (fun c -> Cell.id c = new_id) !all with
    | None -> ()
    | Some new_cell ->
      let after =
        if after_id < 0 then None
        else List.find_opt (fun c -> Cell.id c = after_id) !all
      in
      (* Find what was after `after` in the chain before the new cell was appended *)
      let next = match after with
        | Some a -> Cell.next a
        | None ->
          (* Insert at beginning: next is the first cell (tail of reversed all,
             excluding the newly added new_cell) *)
          let old_all = List.filter (fun c -> Cell.id c <> new_id) !all in
          (match List.rev old_all with [] -> None | first :: _ -> Some first)
      in
      Cell.insert_after ~after ~next new_cell
  in
  Jv.set Jv.global "xOcamlInsertAfter"
    (Jv.callback ~arity:2 (fun args ->
      insert_after (Jv.to_int args.(0)) (Jv.to_int args.(1));
      Jv.undefined))
