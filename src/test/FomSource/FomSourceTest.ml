open FomTest
open FomSource

let () =
  let module Id = Id.Make () in
  let x = Id.id Loc.dummy "x" in
  let x'0 = Id.freshen x in
  let x'1 = Id.freshen x'0 in
  test "fresh id not equal to original" (fun () ->
      verify (not (Id.equal x x'0)));
  test "fresh id has dollar value" (fun () -> verify (x'0.it = "x$0"));
  test "fresh id dollar value increases" (fun () -> verify (x'1.it = "x$1"))
