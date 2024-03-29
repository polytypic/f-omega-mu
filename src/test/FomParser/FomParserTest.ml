open Rea
open MuTest
open FomAST
open FomParser

let test_parses_as name source check =
  test name @@ fun () ->
  source
  |> Parser.parse_utf_8 Grammar.mods Lexer.offside
  |> tryin (fun _ -> verify false) check

let () =
  test_parses_as "location info" "Λα:*.\n  λx:α.x" @@ function
  | `Gen
      ( _,
        ({Typ.Var.at = {pos_lnum = 1; pos_bol = 0; pos_cnum = 1; _}, _; _} as
        alpha1),
        `Star _,
        `LamPat
          ( _,
            `Annot
              ( _,
                `Var
                  ( _,
                    ({
                       Exp.Var.at =
                         {pos_lnum = 2; pos_bol = 6; pos_cnum = 9; _}, _;
                       _;
                     } as x1) ),
                `Var (_, alpha2) ),
            `Var (_, x2) ) ) ->
    verify (Typ.Var.to_string alpha1 = "α" && Typ.Var.equal alpha1 alpha2)
    >> verify (Exp.Var.to_string x1 = "x" && 0 = Exp.Var.compare x1 x2)
  | _ -> verify false

let () =
  test_parses_as "symbolic" "Λt.μdiverge:t→t.λx:t.diverge x" @@ function
  | `Gen _ -> unit
  | _ -> verify false

let () =
  test_parses_as "keywords"
    "gen t => rec diverge : t -> t => fun x : t => diverge x"
  @@ function
  | `Gen _ -> unit
  | _ -> verify false

let () =
  test "parse error" @@ fun () ->
  ""
  |> Parser.parse_utf_8 Grammar.mods Lexer.offside
  |> tryin (fun _ -> pure ()) (fun _ -> verify false)
