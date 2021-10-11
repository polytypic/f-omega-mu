open FomBasis
open FomTest

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
  |> List.iter_fr @@ fun (to_canonize, expected) ->
     let actual = Filename.canonic to_canonize in
     if actual <> expected then
       failuref "Expected: %s\nActual:   %s\n" expected actual
     else
       unit

let () =
  test "List.map_fr" @@ fun () ->
  let xs = [3; 1; 4; 1] in
  let* ys = xs |> List.map_fr (( + ) 1 >>> return) in
  verify (List.map (( + ) 1) xs = ys)
