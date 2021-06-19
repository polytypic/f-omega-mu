open FomBasis
open FomAST
open FomTest
open FomParser

(* *)

open Rea

let test_parses_as name source check =
  test name @@ fun () ->
  source
  |> Parser.parse_utf_8 Grammar.program Lexer.plain
  |> try_in check @@ fun _ -> verify false

let () =
  test_parses_as "location info" "Λα:*.\n  λx:α.x" @@ function
  | `Gen
      ( _,
        ({Typ.Id.at = {pos_lnum = 1; pos_bol = 0; pos_cnum = 1; _}, _; _} as
        alpha1),
        `Star _,
        `LamPat
          ( _,
            `Id
              ( _,
                ({
                   Exp.Id.at = {pos_lnum = 2; pos_bol = 6; pos_cnum = 9; _}, _;
                   _;
                 } as x1),
                `Var (_, alpha2) ),
            `Var (_, x2) ) ) ->
    verify (Typ.Id.to_string alpha1 = "α" && Typ.Id.equal alpha1 alpha2)
    >> verify (Exp.Id.to_string x1 = "x" && 0 = Exp.Id.compare x1 x2)
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
