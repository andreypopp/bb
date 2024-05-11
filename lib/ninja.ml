open Printf

let pf out fmt = ksprintf (fprintf out "%s\n") fmt
let set out n v = pf out "%s = %s" n v
let set_path out n (v : P.t) = pf out "%s = %s" n (v :> string)
let subninja out (path : P.t) = pf out "subninja %s" (path :> string)

let rule out ~command n =
  pf out "rule %s" n;
  pf out "  command = %s" command

let build :
    out_channel ->
    string ->
    ?implicit_out:P.t list ->
    ?implicit_inp:P.t list ->
    ?dyndep:P.t ->
    inp:P.t list ->
    P.t ->
    unit =
 fun out rule ?implicit_out ?implicit_inp ?dyndep ~inp output ->
  let implicit : P.t list option -> string = function
    | None -> ""
    | Some x -> String.concat " " (" |" :: (x :> string list))
  in
  let dyndep' =
    match dyndep with None -> "" | Some x -> sprintf " || %s" (x :> string)
  in
  let inp = String.concat " " (inp :> string list) in
  pf out "build %s%s : %s %s%s%s"
    (output :> string)
    (implicit implicit_out) rule inp (implicit implicit_inp) dyndep';
  match dyndep with
  | None -> ()
  | Some x -> pf out "  dyndep = %s" (x :> string)

let phony : out_channel -> string -> P.t list -> unit =
 fun out name targets ->
  build out "phony" ~implicit_inp:targets ~inp:[] (P.v name)
