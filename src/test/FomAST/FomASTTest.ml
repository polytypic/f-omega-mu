open FomBasis
open FomTest

let parse_typ utf_8 =
  let open FomParser in
  Buffer.from_utf_8 utf_8 |> parse Grammar.typ_exp Lexer.plain

let () =
  test "Typ.to_string" @@ fun () ->
  let original = "∀x:*.μxs.(x→(x→x))→xs" in
  let formatted =
    parse_typ original |> FomElab.elaborate_typ
    |> Reader.run (FomEnv.Env.empty ())
    |> FomAST.Typ.pp |> FomPP.to_string
  in
  verify (formatted = "∀x.μxs.(x → x → x) → xs")
