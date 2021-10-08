type ('tok, 'a) monad =
  | Monad of
      ((Buffer.t -> 'tok) ->
      Buffer.t ->
      Lexing.position ->
      bool ->
      'tok option ->
      ('tok, 'a) result * bool * 'tok option)

and ('tok, 'a) result = Emit of 'tok * ('tok, 'a) monad | Return of 'a

let return value =
  Monad (fun _ _ _ is_typ tok_opt -> (Return value, is_typ, tok_opt))

let unit = Monad (fun _ _ _ is_typ tok_opt -> (Return (), is_typ, tok_opt))

let rec ( >>= ) (Monad xM) xyM =
  Monad
    (fun get_tok buffer last_pos is_typ tok_opt ->
      match xM get_tok buffer last_pos is_typ tok_opt with
      | Emit (tok, xM), is_typ, tok_opt ->
        (Emit (tok, xM >>= xyM), is_typ, tok_opt)
      | Return x, is_typ, tok_opt ->
        let (Monad yM) = xyM x in
        yM get_tok buffer last_pos is_typ tok_opt)

let ( let* ) = ( >>= )
let ( >> ) lhs rhs = lhs >>= fun () -> rhs

(* *)

let col_of (_, (p : Lexing.position), _) = p.pos_cnum - p.pos_bol
let tok_of (t, _, _) = t

let set token (_, s, (e : Lexing.position)) =
  (token, s, {e with pos_bol = e.pos_bol - 1})

(* *)

let get =
  Monad
    (fun get_tok buffer _ is_typ tok_opt ->
      ( Return (match tok_opt with Some tok -> tok | None -> get_tok buffer),
        is_typ,
        None ))

let unget tok =
  Monad
    (fun _ _ _ is_typ tok_opt ->
      match tok_opt with
      | Some _ -> failwith "unget"
      | None -> (Return (), is_typ, Some tok))

let loc =
  Monad
    (fun _ buffer _ is_typ tok_opt ->
      (Return (Buffer.loc buffer), is_typ, tok_opt))

let emit tok =
  Monad (fun _ _ _ is_typ tok_opt -> (Emit (tok, unit), is_typ, tok_opt))

let emit_if bool tok = if bool then emit tok else unit
let emit_before tok token = unget tok >> emit (set token tok)

let new_line (_, (p : Lexing.position), _) =
  Monad
    (fun _ _ last_pos is_typ tok_opt ->
      (Return (last_pos.pos_bol <> p.pos_bol), is_typ, tok_opt))

let is_typ =
  Monad (fun _ _ _ is_typ tok_opt -> (Return is_typ, is_typ, tok_opt))

let set_is_typ is_typ =
  Monad (fun _ _ _ _ tok_opt -> (Return (), is_typ, tok_opt))

let as_typ op =
  let* was = is_typ in
  set_is_typ true >> op >>= fun res -> set_is_typ was >> return res

let with_indent rule = get >>= fun tok -> rule (col_of tok) tok

(* *)

type 'tok state =
  (('tok, unit) monad * Lexing.position * bool * 'tok option) ref

let init token start buffer =
  let state = ref (start, Lexing.dummy_pos, false, None) in
  fun () ->
    let Monad uM, last_pos, is_typ, tok_opt = !state in
    match uM token buffer last_pos is_typ tok_opt with
    | Emit (((_, _, last_pos) as tok), continue), is_typ, tok_opt ->
      state := (continue, last_pos, is_typ, tok_opt);
      tok
    | Return (), _, _ -> failwith "return"
