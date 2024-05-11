open Printf

type t = string

let normalize = List.filter (function "" | "." | ".." -> false | _ -> true)

let v p =
  let xs = String.split_on_char '/' p in
  let xs =
    match xs with
    | (("" | "." | "..") as x) :: xs -> x :: normalize xs
    | xs -> normalize xs
  in
  String.concat "/" xs

let vf fmt = Printf.ksprintf v fmt
let join x y = v (Filename.concat x y)
let ( / ) = join
let rem_ext p = Filename.remove_extension p

let add_ext ext p =
  assert (String.length ext > 0 && ext.[0] = '.');
  sprintf "%s%s" p ext

let mod_ext ext p = add_ext ext (rem_ext p)
let is_abs p = not (Filename.is_relative p)
let is_rel p = Filename.is_relative p
let cwd () = v (Sys.getcwd ())
let resolve p = if is_abs p then p else cwd () / p

let relative ?to_ p =
  let to_ = match to_ with None -> cwd () | Some x -> x in
  let p = resolve p in
  if String.starts_with p ~prefix:to_ then
    v ("./" ^ String.(sub p (length to_) (length p - length to_)))
  else p

let dirname p = Filename.dirname p

module Set = Set.Make (String)
