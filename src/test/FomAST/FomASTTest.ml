open FomBasis
open FomPPrint
open FomParser
open FomTest

let parse_typ utf_8 =
  Buffer.from_utf_8 utf_8 |> Parser.parse Grammar.sigs Lexer.offside

let () =
  test "Typ.to_string" @@ fun () ->
  let original = "∀x:*.μxs:*.(x→(x→x))→xs" in
  parse_typ original >>= FomElab.elaborate_typ
  |> with_env (ignore >>> FomEnv.Env.empty)
  >>- FomPP.Typ.pp >>- to_string
  |> try_in
       (fun formatted -> verify (formatted = "∀x.μxs.(x → x → x) → xs"))
       (fun _ -> verify false)

let () =
  let open JsonString in
  test "LitString" @@ fun () ->
  verify
    (to_utf8 @@ of_utf8 "foo\tbar\n"
    = to_utf8 @@ of_utf8_json "\"foo\\tbar\\n\"")
