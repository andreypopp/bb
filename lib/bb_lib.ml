module P = P

let sf fmt = Printf.sprintf fmt
let errf fmt = Printf.ksprintf (fun s -> failwith s) fmt

let rec mkdirp (path : P.t) =
  try Unix.mkdir (path :> string) 0o777 with
  | Unix.Unix_error (Unix.EINTR, _, _) -> mkdirp path
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdirp (P.dirname path);
      mkdirp path
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let projectdir = P.v "$projectdir"
let srcdir = P.v "$srcdir"
let builddir = P.v "$builddir"
let objdir = P.v "$objdir"

(** ocaml build rules *)
module Ocaml = struct
  let sf fmt = Printf.sprintf fmt

  let configure ~cflags ~lflags ~ocamlfind_pkgs out =
    let cflags = String.concat " " cflags in
    let lflags = String.concat " " lflags in
    let ocamlfind_pkgs = String.concat " " ocamlfind_pkgs in
    Ninja.rule out "stage" ~command:"cp $in $out";
    Ninja.rule out "ocamldep"
      ~command:"bb ocamldep --objdir $objdir -I $builddir $in > $out";
    Ninja.rule out "ocamldep_order" ~command:"bb ocamlorder $in > $out";
    Ninja.rule out "ocamlopt"
      ~command:(sf "ocamlopt.opt -g -I $objdir %s -c $in -o $out" cflags);
    Ninja.rule out "ocamlmkexe"
      ~command:(sf "ocamlopt.opt -g -I $objdir %s -args $in -o $out" lflags);
    Ninja.rule out "ocamlmklib"
      ~command:(sf "ocamlopt.opt -g -I $objdir %s -args $in -a -o $out" lflags);
    Ninja.rule out "ocamlfind_cflags"
      ~command:
        (sf
           "ocamlfind query -predicates native -i-format %s | sed 's/ /\\n/g' \
            > $out"
           ocamlfind_pkgs);
    Ninja.rule out "ocamlfind_lflags"
      ~command:
        (sf
           "ocamlfind query -predicates native -r -a-format %s | sed 's/ \
            /\\n/g' > $out"
           ocamlfind_pkgs)

  let p_src p = P.(srcdir / p)
  let p_stage p = P.(builddir / p)
  let p_cmx p = P.(objdir / mod_ext ".cmx" p)
  let p_cmi p = P.(objdir / mod_ext ".cmi" p)
  let p_o p = P.(objdir / mod_ext ".o" p)
  let p_a p = P.(builddir / mod_ext ".a" p)
  let p_dep p = P.(objdir / add_ext ".d" p)
  let p_ocamlfind_cflags = P.(objdir / v "ocamlfind.cflags")
  let p_ocamlfind_lflags = P.(objdir / v "ocamlfind.lflags")
  let link_order n = P.(objdir / vf "%s.order" n)
  let stage out p = Ninja.build out "stage" (p_stage p) ~inp:[ p_src p ]
  let ocamldep out p = Ninja.build out "ocamldep" (p_dep p) ~inp:[ p_stage p ]

  let ocamldep_order out n srcs =
    let deps = List.map p_dep srcs in
    Ninja.build out "ocamldep_order" (link_order n) ~inp:deps

  let ocamlopt ~deps out p =
    Ninja.build out "ocamlopt" (p_cmx p)
      ~implicit_out:[ p_cmi p; p_o p ]
      ~inp:[ p_stage p ]
      ~implicit_inp:deps ~dyndep:(p_dep p)

  let ocamlmkexe out n srcs =
    let cmxs = List.map p_cmx srcs in
    let exe = P.(builddir / vf "%s.exe" n) in
    Ninja.build out "ocamlmkexe" exe ~inp:[ link_order n ] ~implicit_inp:cmxs;
    exe

  let ocamlmklib out n srcs =
    let cmxs = List.map p_cmx srcs in
    let cmxa = P.(builddir / vf "%s.cmxa" n) in
    let a = P.mod_ext ".a" cmxa in
    Ninja.build out "ocamlmklib" cmxa
      ~inp:[ link_order n ]
      ~implicit_inp:cmxs ~implicit_out:[ a ];
    cmxa

  let compile out ~deps ~cflags ~lflags ~srcs ~ocamlfind_pkgs
      ~link_ocamlfind_pkgs =
    Ninja.set_path out "objdir" P.(builddir / v "_ocaml");
    let deps, cflags, lflags =
      match ocamlfind_pkgs with
      | [] -> (deps, cflags, lflags)
      | _ ->
          ( p_ocamlfind_cflags :: p_ocamlfind_lflags :: deps,
            sf "-args %s" (p_ocamlfind_cflags :> string) :: cflags,
            if link_ocamlfind_pkgs then
              sf "-args %s" (p_ocamlfind_lflags :> string) :: lflags
            else lflags )
    in
    configure ~cflags ~lflags ~ocamlfind_pkgs out;
    (match ocamlfind_pkgs with
    | [] -> ()
    | _ ->
        Ninja.build out "ocamlfind_cflags" p_ocamlfind_cflags ~inp:[];
        Ninja.build out "ocamlfind_lflags" p_ocamlfind_lflags ~inp:[]);
    List.iter
      (fun p ->
        stage out p;
        ocamldep out p;
        ocamlopt ~deps out p)
      srcs
end

type deps = [ `lib of string | `ocamlfind_lib of string | `path of P.t ] list

module Lib = struct
  type t = {
    path : P.t;
    name : string;
    srcs : P.t list;
    deps : deps;
    cflags : string list;
    lflags : string list;
  }
  (** invariant: path should be relative to projectdir *)

  let gen_rules' ~path ~name ~deps ~cflags ~lflags ~srcs ~link_ocamlfind_pkgs
      out =
    Ninja.set_path out "srcdir" P.(projectdir / path);
    Ninja.set_path out "builddir" P.(projectdir / v "_b" / path);
    let deps, ocamlfind_pkgs =
      List.partition_map
        (function
          | `lib _ -> errf "invariant violation: unresolved dep"
          | `ocamlfind_lib x -> Right x
          | `path x -> Left x)
        deps
    in
    Ocaml.compile out ~deps ~ocamlfind_pkgs ~link_ocamlfind_pkgs ~cflags ~lflags
      ~srcs;
    Ocaml.ocamldep_order out name srcs

  let gen_rules { path; name; srcs; cflags; lflags; deps } out =
    gen_rules' ~path ~name ~deps ~cflags ~lflags ~srcs
      ~link_ocamlfind_pkgs:false out;
    let cmxa = Ocaml.ocamlmklib out name srcs in
    Ninja.phony out (P.(path / vf "%s.cmxa" name) :> string) [ cmxa ]

  let archive t = P.(projectdir / v "_b" / t.path / vf "%s.cmxa" t.name)
  let objdir t = P.(projectdir / v "_b" / t.path / v "_ocaml")
end

module Exe = struct
  type t = {
    path : P.t;
    name : string;
    srcs : P.t list;
    deps : deps;
    cflags : string list;
    lflags : string list;
  }
  (** invariant: path should be relative to projectdir *)

  let gen_rules { name; srcs; path; cflags; lflags; deps } out =
    Lib.gen_rules' ~path ~name ~deps ~cflags ~lflags ~srcs
      ~link_ocamlfind_pkgs:true out;
    let exe = Ocaml.ocamlmkexe out name srcs in
    Ninja.phony out (P.(path / vf "%s.exe" name) :> string) [ exe ]
end

module Project = struct
  type t = {
    proj_path : P.t;
    mutable libs : Lib.t list;
    mutable exes : Exe.t list;
  }

  let gen_rules t out =
    Ninja.set_path out "projectdir" t.proj_path;
    Ninja.set_path out "builddir" P.(projectdir / v "_b");
    let ninja_build p =
      let p = P.relative ~to_:t.proj_path p in
      P.(builddir / p / v "build.ninja")
    in
    List.iter
      (fun { Lib.path; _ } -> Ninja.subninja out (ninja_build path))
      t.libs;
    List.iter
      (fun { Exe.path; _ } -> Ninja.subninja out (ninja_build path))
      t.exes

  let find_lib proj name =
    let lib = List.find_opt (fun { Lib.name; _ } -> name = name) proj.libs in
    match lib with None -> errf "no such lib: %s" name | Some lib -> lib

  module Deps = struct
    type state = {
      mutable cflags : string list;
      mutable archives : P.t list;
      mutable deps : [ `ocamlfind_lib of string | `path of P.t ] list;
      mutable seen : P.Set.t;
    }

    let create () =
      { cflags = []; archives = []; deps = []; seen = P.Set.empty }

    let rec resolve state proj (name, deps) =
      if P.Set.mem name state.seen then ()
      else (
        state.seen <- P.Set.add name state.seen;
        List.iter
          (function
            | (`ocamlfind_lib _ | `path _) as dep ->
                state.deps <- dep :: state.deps
            | `lib name ->
                let lib = find_lib proj name in
                resolve state proj (lib.path, lib.deps);
                let objdir = Lib.objdir lib in
                let archive = Lib.archive lib in
                state.deps <- `path archive :: state.deps;
                state.cflags <- sf "-I %s" (objdir :> string) :: state.cflags;
                state.archives <- archive :: state.archives)
          deps)

    let resolve proj deps =
      let state = create () in
      resolve state proj deps;
      ((state.deps :> deps), state.cflags, state.archives)
  end

  let configure_exe t (exe : Exe.t) =
    let deps, cflags, archives = Deps.resolve t (exe.path, exe.deps) in
    { exe with deps; cflags; lflags = (archives :> string list) }

  let configure_lib t (lib : Lib.t) =
    let deps, cflags, _archivs = Deps.resolve t (lib.path, lib.deps) in
    { lib with deps; cflags }

  let configure t =
    t.exes <- List.map (configure_exe t) t.exes;
    t.libs <- List.map (configure_lib t) t.libs

  let dir t = t.proj_path
  let builddir t = P.(t.proj_path / v "_b")
end

let project' : Project.t option ref = ref None
let project () = Option.get !project'
let current_path = ref (P.cwd ())

(* define a library *)
let library ?(cflags = []) ?(lflags = []) ?(deps = []) ?path ~srcs name =
  let proj = project () in
  let path = Option.value path ~default:!current_path in
  let path = P.relative ~to_:(Project.dir proj) path in
  proj.libs <- { Lib.path; name; srcs; deps; cflags; lflags } :: proj.libs

(* define an executable *)
let executable ?(cflags = []) ?(lflags = []) ?path ?(deps = []) ~srcs name =
  let proj = project () in
  let path = Option.value path ~default:!current_path in
  let path = P.relative ~to_:(Project.dir proj) path in
  proj.exes <- { Exe.path; name; srcs; deps; cflags; lflags } :: proj.exes

let load path =
  let proj = project () in
  let path = P.(Project.dir proj / path / v "_b.ml") in
  current_path := P.dirname path;
  print_endline (sf "loading: %s" (P.relative path :> string));
  assert (true = Toploop.use_file Format.std_formatter (path :> string))

let configure proj =
  Project.configure proj;
  let gen_build_ninja path f =
    let p = P.relative ~to_:(Project.dir proj) path in
    let p = P.(Project.builddir proj / p / v "build.ninja") in
    mkdirp (P.dirname p);
    Out_channel.with_open_bin (p :> string) f
  in
  gen_build_ninja proj.proj_path (Project.gen_rules proj);
  List.iter (fun u -> gen_build_ninja u.Lib.path (Lib.gen_rules u)) proj.libs;
  List.iter (fun u -> gen_build_ninja u.Exe.path (Exe.gen_rules u)) proj.exes

let main ~path units =
  let proj =
    let proj_path = P.resolve path in
    { Project.proj_path; libs = []; exes = [] }
  in
  project' := Some proj;
  print_endline (sf "project: %s" (P.relative proj.proj_path :> string));
  List.iter load units;
  configure proj
