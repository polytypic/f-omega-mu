let n_tests = ref 0
let n_successes = ref 0
let n_failures = ref 0

let () =
  at_exit @@ fun () ->
  if !n_failures = 0 then
    Printf.printf "Ran %d tests.  No failures.\n" !n_tests
  else
    Printf.printf "Ran %d tests:\n- %d successes, and\n- %d failures.\n"
      !n_tests !n_successes !n_failures

let inc x = x := !x + 1

let test name effect =
  inc n_tests;
  match
    try
      effect ();
      None
    with exn -> Some exn
  with
  | None ->
    inc n_successes;
    Printf.printf "  [OK] %s\n" name
  | Some exn ->
    inc n_failures;
    Printf.printf "[FAIL] %s: %s\n" name (Printexc.to_string exn)

let verify b = if not b then failwith "ERROR"
