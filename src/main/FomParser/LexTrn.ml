open FomBasis
open FomSource

type 't t = 't * Pos.t * Pos.t
type 't env = (Buffer.t -> 't t) * Buffer.t
type 't state = bool * 't t option

type ('t, 'a) m = 't env -> Pos.t -> 't state -> ('t, 'a) result * 't state

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

  let rec ( let* ) xM xyM env last_pos state =
    match xM env last_pos state with
    | Emit (tok, xM), state -> (Emit (tok, ( let* ) xM xyM), state)
    | Return x, state ->
      let yM = xyM x in
      yM env last_pos state
  in

  object
    method map : 'a 'b. ('a, 'b, _) Functor.map =
      fun xy xF ->
        inj
          (let* x = prj xF in
           return (xy x))

    method return : 'a. ('a, _) Applicative.return = return >>> inj

    method pair : 'a 'b. ('a, 'b, _) Applicative.pair =
      fun xF yF ->
        inj
          (let* x = prj xF in
           let* y = prj yF in
           return (x, y))

    method bind : 'a 'b. ('a, 'b, _) Monad.bind =
      fun xyF xF ->
        inj
          (let* x = prj xF in
           prj (xyF x))
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
let emit_before tok token = unget tok >> emit (set token tok)

(* *)

let is_typ _ = inj @@ fun _ _ ((is_typ, _) as state) -> (Return is_typ, state)

let set_is_typ is_typ _ =
  inj @@ fun _ _ (_, tok_opt) -> (Return (), (is_typ, tok_opt))

let as_typ op =
  let* was = is_typ in
  set_is_typ true >> op >>= fun res -> set_is_typ was >> return res

(* *)

let loc _ = inj @@ fun (_, buffer) _ state -> (Return (Buffer.loc buffer), state)

let new_line (_, (p : Pos.t), _) _ =
  inj @@ fun _ (last_pos : Pos.t) state ->
  (Return (last_pos.pos_bol <> p.pos_bol), state)

let with_indent rule = get >>= fun tok -> rule (col_of tok) tok

(* *)

let init token start buffer =
  let env = (token, buffer)
  and continue' = ref (start methods |> prj)
  and last_pos' = ref Lexing.dummy_pos
  and state' = ref (false, None) in
  fun () ->
    match !continue' env !last_pos' !state' with
    | Emit (((_, _, last_pos) as tok), continue), state ->
      continue' := continue;
      last_pos' := last_pos;
      state' := state;
      tok
    | Return (), _ -> failwith "return"
