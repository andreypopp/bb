open Bb;;

let path = P.(resolve (v __FILE__) |> dirname) in
main ~path [ P.v "example/lib"; P.v "example/bin" ];;
