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
     else unit

let () =
  test "List.map_fr" @@ fun () ->
  let xs = [3; 1; 4; 1] in
  let* ys = xs |> List.map_fr (( + ) 1 >>> return) in
  verify (List.map (( + ) 1) xs = ys)

let () =
  test "EditDistance" @@ fun () ->
  let actual =
    EditDistance.distances
      ~pat:(UTF.UTF8.to_uchar_array "annuAl")
      ~txt:(UTF.UTF8.to_uchar_array "annealing")
      ~pat_uc:(UTF.UTF8.to_uchar_array "annual")
      ~txt_uc:(UTF.UTF8.to_uchar_array "annealing")
  in
  let expected = [|3; 5; 12; 11; 8; 6; 6; 5; 3; 5; 7; 9|] in
  if
    Array.length actual <> Array.length expected
    || Array.exists2 ( <> ) actual expected
  then
    failuref "Expected: %s\nActual:  %s\n"
      (expected |> Array.map Int.to_string |> Array.to_list
     |> String.concat ", ")
      (actual |> Array.map Int.to_string |> Array.to_list |> String.concat ", ")
  else unit
