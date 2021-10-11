open FomBasis

(* *)

let n_tests = ref 0
let n_successes = ref 0
let n_failures = ref 0

(* *)

let tests : (string * (unit, exn, unit) rea) list ref = ref []

(* *)

let inc x = x := !x + 1
let push_to xs x = xs := x :: !xs

let pop_all xs =
  let all = !xs in
  xs := [];
  all

let () =
  at_exit @@ fun () ->
  pop_all tests |> List.rev
  |> List.iter_fr (fun (name, test) ->
         test
         |> try_in
              (fun () ->
                inc n_successes;
                Printf.printf "  [OK] %s\n" name;
                unit)
              (fun exn ->
                inc n_failures;
                Printf.printf "[FAIL] %s: %s\n" name (Printexc.to_string exn);
                unit))
  >>- (fun () ->
        if !n_failures = 0 then
          Printf.printf "Ran %d tests.  No failures.\n" !n_tests
        else
          Printf.printf "Ran %d tests:\n- %d successes, and\n- %d failures.\n"
            !n_tests !n_successes !n_failures)
  |> start ()

let test name effect =
  let test = unit >>= fun () -> try effect () with exn -> fail exn in
  inc n_tests;
  push_to tests (name, test)

let failure m = fail @@ Failure m
let failuref fmt = Printf.ksprintf failure fmt
let verify b = if not b then failure "verify false" else unit
