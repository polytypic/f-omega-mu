open Rea
open StdlibPlus
open MuTest
open FomPPrint
open FomParser

let parse_typ utf_8 =
  Buffer.from_utf_8 utf_8 |> Parser.parse Grammar.sigs Lexer.offside

let () =
  test "Typ.to_string" @@ fun () ->
  let original = "∀x:*.μxs:*.(x→(x→x))→xs" in
  parse_typ original >>= FomElab.elaborate_typ
  |> mapping_env (fun o ->
         object
           inherit [_, _, _] async'of o
           inherit [_, _, _] FomEnv.Env.empty ()
         end)
  >>- FomPP.Typ.pp >>- to_string
  |> tryin
       (fun _ -> verify false)
       (fun formatted -> verify (formatted = "∀x.μxs.(x → x → x) → xs"))

let () =
  let open JsonString in
  test "LitString" @@ fun () ->
  verify
    (to_utf8 @@ of_utf8 "foo\tbar\n"
    = to_utf8 @@ of_utf8_json "\"foo\\tbar\\n\"")
