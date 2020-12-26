open FomTest
open FomParser

let parse_program = parse_utf_8 Grammar.program Lexer.plain

let () =
  test "location info" @@ fun () ->
  match parse_program "Λα:*.\n  λx:α.x" with
  | `Gen
      ( _,
        {it = "α"; at = {pos_lnum = 1; pos_bol = 0; pos_cnum = 1; _}, _},
        `Star _,
        `Lam
          ( _,
            {it = "x"; at = {pos_lnum = 2; pos_bol = 6; pos_cnum = 9; _}, _},
            `Var (_, {it = "α"; _}),
            `Var (_, {it = "x"; _}) ) ) ->
    ()
  | _ -> verify false

let () =
  test "symbolic" @@ fun () ->
  match parse_program "Λt.μdiverge:t→t.λx:t.diverge x" with
  | `Gen _ -> ()
  | _ -> verify false

let () =
  test "keywords" @@ fun () ->
  match
    parse_program "gen t => rec diverge : t -> t => fun x : t => diverge x"
  with
  | `Gen _ -> ()
  | _ -> verify false
