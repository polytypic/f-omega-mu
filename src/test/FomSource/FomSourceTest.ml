open FomBasis
open FomTest
open FomSource

let () =
  let module Id = Id.Make () in
  let x = Id.of_string Loc.dummy "x" in
  let x'0 = Id.freshen x in
  let x'1 = Id.freshen x'0 in
  test "fresh ids have equal names" (fun () ->
      verify (Id.name x = Id.name x'0) >> verify (Id.name x'0 = Id.name x'1));
  test "fresh ids are not equal" (fun () ->
      verify (not (Id.equal x x'0))
      >> verify (not (Id.equal x'0 x'1))
      >> verify (not (Id.equal x'1 x)))
