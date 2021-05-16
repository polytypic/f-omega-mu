open FomBasis
open FomTest

(* *)

open Rea

let () =
  test "FilenameExt.canonic" @@ fun () ->
  [
    ("./foo", "foo");
    ("/./foo", "/foo");
    ("foo/../bar", "bar");
    ("foo/../../../bar", "../../bar");
    ("/foo/./bar//baz", "/foo/bar//baz");
    ("/foo/./bar//../baz", "/foo/bar//../baz");
    ("/foo/bar/../baz", "/foo/baz");
    ("/foo/bar/../../baz", "/baz");
    ("/foo/bar/../../../baz", "/../baz");
    ("/foo/bar/../../../../baz", "/../../baz");
  ]
  |> MList.iter @@ fun (to_canonize, expected) ->
     let actual = FilenameExt.canonic to_canonize in
     if actual <> expected then (
       Printf.printf "Expected: %s\nActual:   %s\n" expected actual;
       verify false)
     else
       unit
