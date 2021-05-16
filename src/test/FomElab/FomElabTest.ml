open FomBasis
open FomAST
open FomSource
open FomTest

(* *)

open Rea

let () =
  test "resolve" @@ fun () ->
  [
    ("/foo/bar.fom", "baz", "/foo/baz");
    ("/foo/bar.fom", "/lol/bal", "/lol/bal");
    ("/foo/bar.fom", "https://lol/foo/../../bal.fom", "https://lol/../bal.fom");
    ("https://host/foo.fom", "https://lol/../bal", "https://lol/../bal");
    ("https://host:80/foo/bar.fom", "../baz", "https://host:80/baz");
    ("https://host:80/foo/bar.fom", "/baz", "https://host:80/baz");
    ("https://host:80/foo/bar.fom", "baz", "https://host:80/foo/baz");
  ]
  |> MList.iter @@ fun (loc, filename, expected) ->
     let actual =
       FomElab.Path.resolve (Loc.of_filename loc) (LitString.of_utf8 filename)
     in
     if actual <> expected then (
       Printf.printf "Expected: %s\nActual:   %s\n" expected actual;
       verify false)
     else
       unit
