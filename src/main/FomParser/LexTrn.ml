open Rea
open StdlibPlus

type 't t = 't * Pos.t * Pos.t
type 't env = (Buffer.t -> 't t) * Buffer.t
type 't state = bool * 't t option

type ('t, 'a) m = 't env -> 't t -> 't state -> ('t, 'a) result * 't state
and ('t, 'a) result = Emit of 't t * ('t, 'a) m | Return of 'a

(* *)

type 't r

external to_rea : ('t, 'a) m -> ('t r, 'e, 'a) s = "%identity"
external of_rea : ('t r, 'e, 'a) s -> ('t, 'a) m = "%identity"

let unit' _ _ state = (Return (), state)

class ['f] methods =
  let rec ( >>= ) xM xyM env last_pos state =
    match xM env last_pos state with
    | Emit (tok, xM), state -> (Emit (tok, xM >>= xyM), state)
    | Return x, state ->
      let yM = xyM x in
      yM env last_pos state
  in
  object (d : 'D)
    inherit ['f r, 'D] monad'd
    method pure' value = to_rea @@ fun _ _ state -> (Return value, state)
    method bind' xF xyF = to_rea (of_rea (xF d) >>= fun x -> of_rea (xyF x d))
  end

(* *)

let left_of (_, (p : Pos.t), _) = p.pos_cnum - p.pos_bol
let right_of (_, _, (p : Pos.t)) = p.pos_cnum - p.pos_bol
let tok_of (t, _, _) = t
let set token (_, s, (e : Pos.t)) = (token, s, {e with pos_bol = e.pos_bol - 1})

(* *)

let get _ =
  to_rea @@ fun (get_tok, buffer) _ -> function
  | is_typ, Some tok -> (Return tok, (is_typ, None))
  | state -> (Return (get_tok buffer), state)

let unget tok _ =
  to_rea @@ fun _ _ (is_typ, tok_opt) ->
  match tok_opt with
  | Some _ -> failwith "unget"
  | None -> (Return (), (is_typ, Some tok))

(* *)

let emit tok _ = to_rea @@ fun _ _ state -> (Emit (tok, unit'), state)
let emit_if bool tok = if bool then emit tok else unit
let emit_before token tok = unget tok >> emit (set token tok)

(* *)

let is_typ _ = to_rea @@ fun _ _ ((is_typ, _) as state) -> (Return is_typ, state)

let set_is_typ is_typ _ =
  to_rea @@ fun _ _ (_, tok_opt) -> (Return (), (is_typ, tok_opt))

let as_typ op =
  let* was = is_typ in
  set_is_typ true >> op >>= fun res -> set_is_typ was >> pure res

(* *)

let loc _ =
  to_rea @@ fun (_, buffer) _ state -> (Return (Buffer.loc buffer), state)

let last_tok _ = to_rea @@ fun _ last_tok state -> (Return last_tok, state)

let new_line (_, (p : Pos.t), _) _ =
  to_rea @@ fun _ (_, _, (last_pos : Pos.t)) state ->
  (Return (last_pos.pos_bol <> p.pos_bol), state)

let with_indent rule m =
  to_rea @@ fun env last_pos state ->
  match of_rea (get m) env last_pos state with
  | Return tok, state -> of_rea (rule (left_of tok) tok m) env tok state
  | _ -> failwith "with_indent"

(* *)

let init token start buffer =
  let tok = token buffer in
  let env = (token, buffer)
  and continue' = ref @@ of_rea @@ start @@ new methods
  and last_tok' = ref tok
  and state' = ref (false, Some tok) in
  fun () ->
    match !continue' env !last_tok' !state' with
    | Emit (tok, continue), state ->
      continue' := continue;
      last_tok' := tok;
      state' := state;
      tok
    | Return (), _ -> failwith "return"
