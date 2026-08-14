let all = Notebook_ops.all
let find_by_id id = List.find (fun t -> Cell.id t = id) !all

let current_script =
  Brr.El.of_jv (Jv.get (Brr.Document.to_jv Brr.G.document) "currentScript")

let current_attribute attr = Brr.El.at (Jstr.of_string attr) current_script

let extra_load =
  match current_attribute "src-load" with
  | None -> None
  | Some url -> Some (Jstr.to_string url)

let worker_url =
  match current_attribute "src-worker" with
  | None -> failwith "x-ocaml script missing src-worker attribute"
  | Some url -> Jstr.to_string url

let worker = Client.make ?extra_load worker_url

let () =
  Client.on_message worker @@ function
  | Formatted_source (id, code_fmt) -> Cell.set_source (find_by_id id) code_fmt
  | Top_response_at (id, loc, msg) -> Cell.add_message (find_by_id id) loc msg
  | Top_response (id, msg) -> Cell.completed_run (find_by_id id) msg
  | Merlin_response (id, msg) -> Cell.receive_merlin (find_by_id id) msg

let () = Client.post worker Setup

let () =
  match current_attribute "x-ocamlformat" with
  | None -> ()
  | Some conf -> Client.post worker (Format_config (Jstr.to_string conf))

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.of_string "x-ocaml"
  | Some name -> name

let extra_style = current_attribute "src-style"
let inline_style = current_attribute "inline-style"
let run_on = current_attribute "run-on" |> Option.map Jstr.to_string
let run_on_of_string = function "click" -> `Click | "load" | _ -> `Load

let page_has_cached_output =
  lazy
    (Jv.to_int
       (Jv.get
          (Jv.call
             (Jv.get Jv.global "document")
             "querySelectorAll"
             [| Jv.of_string "x-ocaml-output" |])
          "length")
     > 0)

let query_cached_output cell_id =
  let doc = Jv.get Jv.global "document" in
  let selector =
    Printf.sprintf "x-ocaml-output[for=\"%s\"]" cell_id
  in
  let nodes =
    Jv.call doc "querySelectorAll" [| Jv.of_string selector |]
  in
  let len = Jv.to_int (Jv.get nodes "length") in
  let acc = ref [] in
  for i = 0 to len - 1 do
    let node = Jv.call nodes "item" [| Jv.of_int i |] in
    let loc =
      match
        Jv.to_option Jv.to_string
          (Jv.call node "getAttribute" [| Jv.of_string "loc" |])
      with
      | Some s -> (try int_of_string s with _ -> 0)
      | None -> 0
    in
    let typ =
      match
        Jv.to_option Jv.to_string
          (Jv.call node "getAttribute" [| Jv.of_string "type" |])
      with
      | Some s -> s
      | None -> "meta"
    in
    let content = Jstr.to_string (Jv.to_jstr (Jv.get node "innerHTML")) in
    let output =
      match typ with
      | "stdout" -> X_protocol.Stdout content
      | "stderr" -> X_protocol.Stderr content
      | "html" -> X_protocol.Html content
      | _ -> X_protocol.Meta content
    in
    acc := (loc, output) :: !acc
  done;
  List.rev !acc

let load_cached_output editor cached =
  let grouped = Hashtbl.create 16 in
  List.iter
    (fun (loc, output) ->
      let prev =
        match Hashtbl.find_opt grouped loc with Some l -> l | None -> []
      in
      Hashtbl.replace grouped loc (output :: prev))
    cached;
  Hashtbl.iter
    (fun loc msgs -> Cell.add_message editor loc (List.rev msgs))
    grouped

let _ =
  Webcomponent.define elt_name @@ fun this ->
  let prev = match !all with [] -> None | e :: _ -> Some e in
  let run_on =
    run_on_of_string
    @@
    match Webcomponent.get_attribute this "run-on" with
    | Some s -> s
    | None ->
        let default =
          if Lazy.force page_has_cached_output then "click" else "load"
        in
        Option.value ~default run_on
  in
  let id = !Notebook_ops.id_counter in
  incr Notebook_ops.id_counter;
  let cell_id =
    match Webcomponent.get_attribute this "id" with
    | Some s -> s
    | None ->
        let s = Printf.sprintf "x-ocaml-%d" id in
        ignore
          (Jv.call (Jv.repr this) "setAttribute"
             [| Jv.of_string "id"; Jv.of_string s |]);
        s
  in
  ignore (Jv.call (Jv.repr this) "setAttribute"
    [| Jv.of_string "data-ocaml-id"; Jv.of_string (string_of_int id) |]);
  let cached = query_cached_output cell_id in
  let has_cache = cached <> [] in
  let editor =
    Cell.init ~id ~run_on ?extra_style ?inline_style worker this
  in
  if has_cache then load_cached_output editor cached;
  all := editor :: !all;
  Cell.set_prev ~prev editor;
  if List.for_all Cell.loadable !all then Cell.run editor;
  ()

let () = Notebook_ops.setup ()
