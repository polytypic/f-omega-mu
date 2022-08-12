open Rea
open MuTest
open FomParser
open FomElab

let parse_exp source and_then =
  source
  |> Parser.parse_utf_8 Grammar.mods Lexer.offside
  >>= elaborate
  |> tryin (fun _ -> verify false) and_then
  |> mapping_env @@ fun o ->
     object
       inherit [_, _, _] async'of o
       inherit [_, _, _] FomEnv.Env.empty ()
     end

let testCompiles name exp =
  test name @@ fun () ->
  parse_exp exp @@ fun (ast, _, _) ->
  let* _ =
    ast |> FomToJs.erase |> FomToJs.simplify >>= FomToJs.to_js ~top:`Top
  in
  verify true

let () =
  testCompiles "fix via μ type unapplied"
    {|
    let Z = Λa.Λb.
      type μt = t → a → b
      λf:(a → b) → a → b.(λg:t.g g) λx:t.λn:a.f (x x) n
    let fact = Z«int»«int» λfact:int → int.
      λn:int.if n ≤ 0 then 1 else n*fact(n-1)
    fact
    |};
  testCompiles "fix via μ type applied"
    {|
    let Z = Λa.Λb.
      type μt = t → a → b
      λf:(a → b) → a → b.(λg:t.g g) λx:t.λn:a.f (x x) n
    let fact = Z«int»«int» λfact:int → int.
      λn:int.if n ≤ 0 then 1 else n*fact(n-1)
    fact 5
    |};
  testCompiles "non-terminating fix"
    {|
    let Z = Λa.Λb.λf:(a → b) → a → b.
      let z = λx:μt.t → a → b.f (x x)
      z z
    let fact = Z«int»«int» λfact:int → int.
      λn:int.if n ≤ 0 then 1 else n*fact(n-1)
    fact 5
    |};
  testCompiles "inf non-terminating fix"
    {|
    let Z = Λa.Λb.λf:(a → b) → a → b.
      let z = λx:μt.t → a → b.f (x x)
      z z
    let fact = Z«int»«int» λfact:int → int.
      λn:int.if n =«int» 0 then 1 else n*fact(n-1)
    fact (-5)
    |};
  ()
