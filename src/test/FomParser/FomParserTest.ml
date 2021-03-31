open FomAST
open FomTest
open FomParser

let parse_program = parse_utf_8 Grammar.program Lexer.plain

let () =
  test "location info" @@ fun () ->
  match parse_program "Λα:*.\n  λx:α.x" with
  | `Gen
      ( _,
        ({at = {pos_lnum = 1; pos_bol = 0; pos_cnum = 1; _}, _; _} as alpha1),
        `Star _,
        `LamPat
          ( _,
            `Id
              ( _,
                ({at = {pos_lnum = 2; pos_bol = 6; pos_cnum = 9; _}, _; _} as
                x1),
                `Var (_, alpha2) ),
            `Var (_, x2) ) ) ->
    verify (Typ.Id.to_string alpha1 = "α" && Typ.Id.equal alpha1 alpha2);
    verify (Exp.Id.to_string x1 = "x" && 0 = Exp.Id.compare x1 x2)
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
