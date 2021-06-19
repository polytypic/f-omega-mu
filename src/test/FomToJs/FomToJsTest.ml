open FomBasis
open FomTest
open FomParser
open FomElab

(* *)

open Rea

let parse_exp source and_then =
  source
  |> Parser.parse_utf_8 Grammar.program Lexer.plain
  >>= elaborate
  |> with_env (ignore >>> FomEnv.Env.empty)
  |> try_in and_then @@ fun _ -> verify false

let testCompiles name exp =
  test name @@ fun () ->
  parse_exp exp @@ fun ast ->
  let* _ = FomToJs.to_js ast in
  verify true

let () =
  testCompiles "fix via μ type unapplied"
    {eof|
    let Z = Λa.Λb.
      let type μt = t → a → b in
      λf:(a → b) → a → b.(λg:t.g g) (λx:t.λn:a.f (x x) n) in
    let fact = Z[int][int] (λfact:int → int.
      λn:int.if n ≤ 0 then 1 else n*fact(n-1)) in
    fact
    |eof};
  testCompiles "fix via μ type applied"
    {eof|
    let Z = Λa.Λb.
      let type μt = t → a → b in
      λf:(a → b) → a → b.(λg:t.g g) (λx:t.λn:a.f (x x) n) in
    let fact = Z[int][int] (λfact:int → int.
      λn:int.if n ≤ 0 then 1 else n*fact(n-1)) in
    fact 5
    |eof};
  testCompiles "non-terminating fix"
    {eof|
    let Z = Λa.Λb.λf:(a → b) → a → b.
      let z = λx:μt.t → a → b.f (x x) in
      z z in
    let fact = Z[int][int] (λfact:int → int.
      λn:int.if n ≤ 0 then 1 else n*fact(n-1)) in
    fact 5
    |eof}
