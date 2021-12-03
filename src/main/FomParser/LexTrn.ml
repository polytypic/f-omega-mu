open FomBasis
open FomSource

type 't t = 't * Pos.t * Pos.t
type 't env = (Buffer.t -> 't t) * Buffer.t
type 't state = bool * 't t option

type ('t, 'a) m = 't env -> 't t -> 't state -> ('t, 'a) result * 't state

and ('t, 'a) result = Emit of 't t * ('t, 'a) m | Return of 'a

(* *)

include
  Higher.New'2
    (struct
      type nonrec ('t, 'a) t = ('t, 'a) m
    end)
    ()

type ('t, 'a) fr = ('t, f) app'1 Monad.t -> ('t, 'a, f) app'2

let unit' _ _ state = (Return (), state)

let methods =
  let return value _ _ state = (Return value, state) in

  let rec ( >>= ) xM xyM env last_pos state =
    match xM env last_pos state with
    | Emit (tok, xM), state -> (Emit (tok, xM >>= xyM), state)
    | Return x, state ->
      let yM = xyM x in
      yM env last_pos state
  in

  object
    method map : 'a 'b. ('a, 'b, _) Functor.map =
      fun xy xF -> inj (prj xF >>= (xy >>> return))

    method return : 'a. ('a, _) Applicative.return = return >>> inj

    method pair : 'a 'b. ('a, 'b, _) Applicative.pair =
      fun xF yF ->
        inj
          ( prj xF >>= fun x ->
            prj yF >>= fun y -> return (x, y) )

    method bind : 'a 'b. ('a, 'b, _) Monad.bind =
      fun xyF xF -> inj (prj xF >>= (xyF >>> prj))
  end

(* *)

let col_of (_, (p : Pos.t), _) = p.pos_cnum - p.pos_bol
let tok_of (t, _, _) = t
let set token (_, s, (e : Pos.t)) = (token, s, {e with pos_bol = e.pos_bol - 1})

(* *)

let get _ =
  inj @@ fun (get_tok, buffer) _ -> function
  | is_typ, Some tok -> (Return tok, (is_typ, None))
  | state -> (Return (get_tok buffer), state)

let unget tok _ =
  inj @@ fun _ _ (is_typ, tok_opt) ->
  match tok_opt with
  | Some _ -> failwith "unget"
  | None -> (Return (), (is_typ, Some tok))

(* *)

let emit tok _ = inj @@ fun _ _ state -> (Emit (tok, unit'), state)
let emit_if bool tok = if bool then emit tok else unit
let emit_before token tok = unget tok >> emit (set token tok)

(* *)

let is_typ _ = inj @@ fun _ _ ((is_typ, _) as state) -> (Return is_typ, state)

let set_is_typ is_typ _ =
  inj @@ fun _ _ (_, tok_opt) -> (Return (), (is_typ, tok_opt))

let as_typ op =
  let* was = is_typ in
  set_is_typ true >> op >>= fun res -> set_is_typ was >> return res

(* *)

let loc _ = inj @@ fun (_, buffer) _ state -> (Return (Buffer.loc buffer), state)
let last_tok _ = inj @@ fun _ last_tok state -> (Return last_tok, state)

let new_line (_, (p : Pos.t), _) _ =
  inj @@ fun _ (_, _, (last_pos : Pos.t)) state ->
  (Return (last_pos.pos_bol <> p.pos_bol), state)

let with_indent rule = get >>= fun tok -> rule (col_of tok) tok

(* *)

let init token start buffer =
  let tok = token buffer in
  let env = (token, buffer)
  and continue' = ref (start methods |> prj)
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
