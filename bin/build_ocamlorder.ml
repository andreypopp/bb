let drop_prefix ~prefix s =
  let prefix_len = String.length prefix in
  let len = String.length s in
  if len < prefix_len then failwith "drop_prefix: string too short"
  else if not (String.starts_with s ~prefix) 
  then failwith "drop_prefix: string does not start with prefix"
  else String.sub s prefix_len (len - prefix_len)

type item = {
  mutable seen : bool;
  target : string;
  deps : string list;
}

let () =
  let args = Array.to_list Sys.argv in
  let args = List.tl args in
  let t = Hashtbl.create 64 in
  List.iter (fun filename ->
    In_channel.with_open_bin filename (fun ic ->
      let rec loop () =
        match input_line ic with
        | "ninja_dyndep_version = 1" -> loop ()
        | line ->
          (match String.split_on_char ':' line with
          | [target; deps] ->
            let target = drop_prefix ~prefix:"build " target in
            let deps = drop_prefix ~prefix:" dyndep | " deps in
            let deps = if deps = "" then [] else String.split_on_char ' ' deps in
            Hashtbl.replace t target {target; deps; seen = false};
            loop ()
          | _ -> failwith "unexpected line")
        | exception End_of_file -> ()
      in
      loop ()
    )
  ) args;
  if Hashtbl.length t > 0 then
  let emit item =
    if item.seen then ()
    else (item.seen <- true; print_endline item.target)
  in
  let rec visit item =
    List.iter (fun target' ->
      let item' = Hashtbl.find t target' in
      visit item'
    ) item.deps;
    emit item
  in
  Seq.iter visit (Hashtbl.to_seq_values t)
