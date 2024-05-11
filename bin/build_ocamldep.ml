let () =
  let args = Array.to_list Sys.argv in
  let objdir, args =
    match args with
    | _ :: objdir :: args -> (objdir, args)
    | _ -> failwith "ERROR: missing objdir"
  in
  let to_obj path =
    let path = Filename.basename path in
    Filename.concat objdir path
  in
  let args = List.map Filename.quote args in
  let args = String.concat " " args in
  let ic =
    Unix.open_process_in (Printf.sprintf "ocamldep -native -one-line %s" args)
  in
  let parse_line line =
    match String.split_on_char ':' line with
    | [ file; deps ] ->
        let file = String.trim file in
        let deps =
          String.split_on_char ' ' deps
          |> List.map String.trim
          |> List.filter (function "" -> false | _ -> true)
          |> List.map to_obj
        in
        (to_obj file, deps)
    | _ -> failwith (Printf.sprintf "ERROR: invalid line: %s" line)
  in
  let rec loop () =
    try
      let line = input_line ic in
      let file, deps = parse_line line in
      print_endline
        (Printf.sprintf "build %s: dyndep | %s" file (String.concat " " deps));
      loop ()
    with End_of_file -> ()
  in
  print_endline "ninja_dyndep_version = 1";
  loop ()
