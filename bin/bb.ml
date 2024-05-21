module P = Bb_lib.P

let commandf = Printf.ksprintf (fun s -> if 0 <> Sys.command s then exit 1)

let ocamldep objdir includes mls =
  let to_obj path =
    match objdir with
    | None -> path
    | Some objdir -> Filename.concat objdir (Filename.basename path)
  in
  let mls = List.map Filename.quote mls in
  let mls = String.concat " " mls in
  let includes = List.map (fun dir -> "-I " ^ dir) includes in
  let includes = String.concat " " includes in
  let ic =
    Unix.open_process_in
      (Printf.sprintf "ocamldep -native -one-line %s %s" includes mls)
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

let drop_prefix ~prefix s =
  let prefix_len = String.length prefix in
  let len = String.length s in
  if len < prefix_len then failwith "drop_prefix: string too short"
  else if not (String.starts_with s ~prefix) then
    failwith "drop_prefix: string does not start with prefix"
  else String.sub s prefix_len (len - prefix_len)

type item = { mutable seen : bool; target : string; deps : string list }

let ocamlorder args =
  let t = Hashtbl.create 64 in
  List.iter
    (fun filename ->
      In_channel.with_open_bin filename (fun ic ->
          let rec loop () =
            match input_line ic with
            | "ninja_dyndep_version = 1" -> loop ()
            | line -> (
                match String.split_on_char ':' line with
                | [ target; deps ] ->
                    let target = drop_prefix ~prefix:"build " target in
                    let deps = drop_prefix ~prefix:" dyndep | " deps in
                    let deps =
                      if deps = "" then [] else String.split_on_char ' ' deps
                    in
                    Hashtbl.replace t target { target; deps; seen = false };
                    loop ()
                | _ -> failwith "unexpected line")
            | exception End_of_file -> ()
          in
          loop ()))
    args;
  if Hashtbl.length t > 0 then
    let emit item =
      if item.seen then ()
      else (
        item.seen <- true;
        print_endline item.target)
    in
    let rec visit item =
      List.iter
        (fun target' ->
          let item' = Hashtbl.find t target' in
          visit item')
        item.deps;
      emit item
    in
    Seq.iter visit (Hashtbl.to_seq_values t)

let build_ninja = function
  | Some dir -> P.(dir / v "_b" / v "build.ninja")
  | None -> P.v "_b/build.ninja"

let build dir =
  Option.iter
    (fun (dir : P.t) ->
      let dir = P.resolve dir in
      print_endline (Printf.sprintf "chdir: %s" (P.relative dir :> string));
      Sys.chdir (dir :> string))
    dir;
  commandf "ninja -f %s" (build_ninja dir :> string)

let clean dir = commandf "ninja -f %s -t clean" (build_ninja dir :> string)

open Cmdliner

let objdir_t =
  let doc = "the directory for object files" in
  Arg.(value & opt (some string) None & info [ "objdir" ] ~docv:"DIR" ~doc)

let mls_t =
  let doc = "OCaml source files to analyze" in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE" ~doc)

let ds_t =
  let doc = "files in ninja dyndep format" in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE" ~doc)

let path =
  let parse s = Ok (P.v s) in
  let print ppf (s : P.t) = Format.pp_print_string ppf (s :> string) in
  Arg.conv (parse, print)

let root_t =
  let doc = "project root" in
  Arg.(value & opt (some path) None & info [ "root"; "R" ] ~docv:"DIR" ~doc)

let include_t =
  let doc = "include directory" in
  Arg.(value & opt_all string [] & info [ "I" ] ~docv:"DIR" ~doc)

let ocamldep_t =
  let doc = "generate a ninja dyndep file for OCaml source files" in
  let info = Cmd.info "ocamldep" ~doc in
  let term = Term.(const ocamldep $ objdir_t $ include_t $ mls_t) in
  Cmd.v info term

let ocamlorder_t =
  let doc = "read ninja dyndep files and get a topo sort" in
  let info = Cmd.info "ocamlorder" ~doc in
  let term = Term.(const ocamlorder $ ds_t) in
  Cmd.v info term

let build_t =
  let doc = "run build" in
  let info = Cmd.info "build" ~doc in
  let term = Term.(const build $ root_t) in
  Cmd.v info term

let clean_t =
  let doc = "clean built artefacts" in
  let info = Cmd.info "clean" ~doc in
  let term = Term.(const clean $ root_t) in
  Cmd.v info term

let main_t =
  let doc = "bb (Blitzbau), a build system" in
  let info = Cmd.info "bb" ~doc in
  Cmd.group info [ build_t; clean_t; ocamldep_t; ocamlorder_t ]

let () = Cmdliner.(exit @@ Cmd.eval main_t)
