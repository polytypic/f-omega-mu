open FomBasis
open FomSource

type 't t = 't * Pos.t * Pos.t

type ('t, 'a) m =
  (Buffer.t -> 't t) ->
  Buffer.t ->
  Pos.t ->
  bool ->
  't t option ->
  ('t, 'a) result * bool * 't t option

and ('t, 'a) result = Emit of 't t * ('t, 'a) m | Return of 'a

(* *)

include
  Higher.New'2
    (struct
      type nonrec ('t, 'a) t = ('t, 'a) m
    end)
    ()

type ('t, 'a) fr = ('t, f) app'1 Monad.t -> ('t, 'a, f) app'2

let unit' _ _ _ is_typ tok_opt = (Return (), is_typ, tok_opt)

let methods =
  let return value _ _ _ is_typ tok_opt = (Return value, is_typ, tok_opt) in

  let rec ( let* ) xM xyM get_tok buffer last_pos is_typ tok_opt =
    match xM get_tok buffer last_pos is_typ tok_opt with
    | Emit (tok, xM), is_typ, tok_opt ->
      (Emit (tok, ( let* ) xM xyM), is_typ, tok_opt)
    | Return x, is_typ, tok_opt ->
      let yM = xyM x in
      yM get_tok buffer last_pos is_typ tok_opt
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
  inj @@ fun get_tok buffer _ is_typ tok_opt ->
  ( Return (match tok_opt with Some tok -> tok | None -> get_tok buffer),
    is_typ,
    None )

let unget tok _ =
  inj @@ fun _ _ _ is_typ tok_opt ->
  match tok_opt with
  | Some _ -> failwith "unget"
  | None -> (Return (), is_typ, Some tok)

(* *)

let emit tok _ =
  inj @@ fun _ _ _ is_typ tok_opt -> (Emit (tok, unit'), is_typ, tok_opt)

let emit_if bool tok = if bool then emit tok else unit
let emit_before tok token = unget tok >> emit (set token tok)

(* *)

let is_typ _ =
  inj @@ fun _ _ _ is_typ tok_opt -> (Return is_typ, is_typ, tok_opt)

let set_is_typ is_typ _ =
  inj @@ fun _ _ _ _ tok_opt -> (Return (), is_typ, tok_opt)

let as_typ op =
  let* was = is_typ in
  set_is_typ true >> op >>= fun res -> set_is_typ was >> return res

(* *)

let loc _ =
  inj @@ fun _ buffer _ is_typ tok_opt ->
  (Return (Buffer.loc buffer), is_typ, tok_opt)

let new_line (_, (p : Pos.t), _) _ =
  inj @@ fun _ _ (last_pos : Pos.t) is_typ tok_opt ->
  (Return (last_pos.pos_bol <> p.pos_bol), is_typ, tok_opt)

let with_indent rule = get >>= fun tok -> rule (col_of tok) tok

(* *)

let init token start buffer =
  let state = ref (start methods |> prj, Lexing.dummy_pos, false, None) in
  fun () ->
    let uM, last_pos, is_typ, tok_opt = !state in
    match uM token buffer last_pos is_typ tok_opt with
    | Emit (((_, _, last_pos) as tok), continue), is_typ, tok_opt ->
      state := (continue, last_pos, is_typ, tok_opt);
      tok
    | Return (), _, _ -> failwith "return"
