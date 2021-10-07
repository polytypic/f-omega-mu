open FomBasis
open FomSource
open FomTest

let () =
  test "Path.coalesce" @@ fun () ->
  [
    ("/foo/bar.fom", "baz", "/foo/baz");
    ("/foo/bar.fom", "/lol/bal", "/lol/bal");
    ("/foo/bar.fom", "https://lol/foo/../../bal.fom", "https://lol/../bal.fom");
    ("https://host/foo.fom", "https://lol/../bal", "https://lol/../bal");
    ("https://host:80/foo/bar.fom", "../baz", "https://host:80/baz");
    ("https://host:80/foo/bar.fom", "/baz", "https://host:80/baz");
    ("https://host:80/foo/bar.fom", "baz", "https://host:80/foo/baz");
  ]
  |> List.iter_fr @@ fun (loc, path, expected) ->
     let actual =
       FomElab.Path.coalesce (Loc.of_path loc) (JsonString.of_utf8 path)
     in
     if actual <> expected then (
       Printf.printf "Expected: %s\nActual:   %s\n" expected actual;
       verify false)
     else
       unit
