open FomAST
open FomSource
open FomTest

let () =
  test "resolve" @@ fun () ->
  [
    ("/foo/bar.fom", "baz", "/foo/baz.fom");
    ("/foo/bar.fom", "/lol/bal", "/lol/bal.fom");
    ("/foo/bar.fom", "https://lol/foo/../../bal.fom", "https://lol/../bal.fom");
    ("https://host/foo.fom", "https://lol/../bal", "https://lol/../bal.fom");
    ("https://host:80/foo/bar.fom", "../baz", "https://host:80/baz.fom");
    ("https://host:80/foo/bar.fom", "/baz", "https://host:80/baz.fom");
    ("https://host:80/foo/bar.fom", "baz", "https://host:80/foo/baz.fom");
  ]
  |> List.iter @@ fun (loc, filename, expected) ->
     let actual =
       FomModules.resolve (Loc.of_filename loc)
         (LitString.of_utf8 filename)
         ~ext:FomModules.mod_ext
     in
     if actual <> expected then (
       Printf.printf "Expected: %s\nActual:   %s\n" expected actual;
       verify false)
