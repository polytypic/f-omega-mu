open FomBasis
open Grammar
open Parser
open LexTrn

let error message = loc >>= fun loc -> raise @@ Exn_lexeme (loc, message)

let expect exp =
  get >>= fun tok -> if tok_of tok <> exp then error "unexpected" else emit tok

(* *)

let is_closing = function
  | And | BraceRhs | BracketRhs | Comma | DoubleAngleQuoteRhs | EOF | Else | In
  | ParenRhs | Then ->
    true
  | _ -> false

let is_bop tok =
  match tok_of tok with
  | Caret | Comma | Diamond | DoubleComma | Equal | Greater | GreaterEqual
  | Less | LessEqual | LogicalAnd | LogicalOr | NotEqual | Percent | Plus
  | Semicolon | Slash | Star | TriangleLhs | TriangleRhs ->
    true
  | _ -> false

let is_continued tok =
  match tok_of tok with Comma | In | Semicolon -> true | _ -> false

let classify indent tok and_then =
  let column = left_of tok in
  if column < indent then and_then `Dedent
  else if column = indent && not (is_bop tok) then
    let* last_tok and* new_line = new_line tok in
    if new_line && not (is_continued last_tok) then and_then `Indent
    else and_then `Inside
  else and_then `Inside

let ns tok tok_ns =
  let* last_tok in
  match tok_of last_tok with
  | (Id _ | BraceRhs | BracketRhs | ParenRhs)
    when right_of last_tok = left_of tok ->
    emit (set tok_ns tok)
  | _ -> emit tok

let rec nest tok =
  (match tok_of tok with
  | ForAll | Exists | MuLower ->
    get >>= fun tok' ->
    if tok_of tok' <> ParenLhs then
      emit (set ParenLhs tok)
      >> unget tok' >> emit tok
      >> as_typ (with_indent (insert_semis emit))
      >> with_indent (insert_semis ~dedent:true (emit_before ParenRhs))
    else unget tok' >> emit tok
  | LambdaLower | LambdaUpper ->
    emit (set ParenLhs tok)
    >> emit tok
    >> as_typ (with_indent (insert_semis emit))
    >> with_indent (insert_semis ~dedent:true (emit_before ParenRhs))
  | DoubleAngleQuoteLhs ->
    emit (set ParenLhs tok)
    >> emit tok
    >> as_typ (with_indent (insert_semis emit))
    >> with_indent (insert_semis emit)
    >> get
    >>= (fun tok -> (if tok_of tok = Colon then nest else return) tok)
    >>= emit_before ParenRhs
  | Include -> emit tok >> with_indent (insert_in `Par)
  | Type -> emit tok >> as_typ (binding tok)
  | Let -> emit tok >> binding tok
  | Colon ->
    emit tok
    >> emit (set ParenLhs tok)
    >> as_typ (with_indent (insert_semis ~dedent:true (emit_before ParenRhs)))
  | BraceLhs -> ns tok BraceLhsNS >> with_indent insert_commas
  | BracketLhs ->
    ns tok BracketLhsNS >> as_typ (with_indent (insert_semis emit))
  | ParenLhs ->
    ns tok ParenLhsNS >> with_indent (insert_semis ~commas:true emit)
  | If ->
    emit (set ParenLhs tok)
    >> emit tok
    >> with_indent (insert_semis emit)
    >> with_indent (insert_semis emit)
    >> with_indent (insert_semis ~dedent:true (emit_before ParenRhs))
  | _ -> emit tok)
  >> get

and insert_commas indent tok =
  match tok_of tok with
  | BraceRhs -> emit tok
  | Comma -> (
    classify indent tok @@ function
    | `Dedent -> error "offside"
    | _ -> emit tok >> get >>= insert_commas indent)
  | _ -> (
    classify indent tok @@ function
    | `Dedent -> error "offside"
    | `Indent -> emit (set Comma tok) >> nest tok >>= insert_commas indent
    | _ -> nest tok >>= insert_commas indent)

and insert_in form indent tok =
  match tok_of tok with
  | EOF -> emit tok
  | And -> (
    classify indent tok @@ function
    | `Dedent -> error "offside"
    | _ ->
      emit tok
      >> (match form with `Rec -> expect MuLower >> get | `Par -> get)
      >>= insert_in form indent)
  | In -> (
    classify indent tok @@ function `Dedent -> error "offside" | _ -> emit tok)
  | _ -> (
    classify indent tok @@ function
    | `Dedent | `Indent -> emit_before In tok
    | _ -> nest tok >>= insert_in form indent)

and binding tok =
  let indent = left_of tok in
  get >>= fun tok ->
  if tok_of tok = MuLower then emit tok >> get >>= insert_in `Rec indent
  else insert_in `Par indent tok

and insert_semis ?(commas = false) ?(dedent = false) on_exit indent tok =
  let* is_typ in
  match tok_of tok with
  | Comma when commas -> (
    classify indent tok @@ function
    | `Dedent -> error "offside"
    | _ -> emit tok >> get >>= insert_semis ~commas ~dedent on_exit indent)
  | t when is_closing t -> on_exit tok
  | (Dot | Equal) when is_typ -> on_exit tok
  | _ -> (
    classify indent tok @@ function
    | `Dedent -> if dedent then on_exit tok else error "offside"
    | `Indent when not is_typ ->
      emit (set Semicolon tok)
      >> nest tok
      >>= insert_semis ~commas ~dedent on_exit indent
    | _ -> nest tok >>= insert_semis ~commas ~dedent on_exit indent)
