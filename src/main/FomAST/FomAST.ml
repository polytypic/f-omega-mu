open FomBasis
open FomPP
open FomSource

module LitString = struct
  type t = string

  let compare = String.compare

  (* *)

  let of_utf8_json = Fun.id
  let to_utf8_json = Fun.id

  (* *)

  let of_utf8 str =
    let buffer = Buffer.create (String.length str * 2) in
    let encoder = Uutf.encoder `UTF_8 @@ `Buffer buffer in
    let encode c = Uutf.encode encoder @@ `Uchar c |> ignore in
    let to_hex i =
      Uchar.of_int
        (i
        +
        if 0 <= i && i <= 9 then
          Uchar.to_int (Uchar.of_char '0')
        else
          Uchar.to_int (Uchar.of_char 'a') - 10)
    in
    encode @@ Uchar.of_char '"';
    str
    |> Uutf.String.fold_utf_8
         (fun i _ -> function
           | `Malformed _ ->
             failwithf "Malformed UTF-8 in string at char index %d" i
           | `Uchar u ->
             let c = Uchar.to_int u in
             if (0x0000 <= c && c <= 0x001f) || (0x007f <= c && c <= 0x009f)
             then (
               encode @@ Uchar.of_char '\\';
               encode @@ Uchar.of_char 'u';
               encode @@ to_hex ((c lsr 12) land 0xf);
               encode @@ to_hex ((c lsr 8) land 0xf);
               encode @@ to_hex ((c lsr 4) land 0xf);
               encode @@ to_hex ((c lsr 0) land 0xf))
             else if Uchar.of_char '"' = u || Uchar.of_char '\\' = u then (
               encode @@ Uchar.of_char '\\';
               encode u)
             else
               encode u;
             i + 1)
         0
    |> ignore;
    encode @@ Uchar.of_char '"';
    Uutf.encode encoder `End |> ignore;
    Buffer.contents buffer

  let to_utf8 lit =
    let buffer = Buffer.create (String.length lit * 2) in
    let encoder = Uutf.encoder `UTF_8 @@ `Buffer buffer in
    let hex_to_int h c =
      (h lsl 4)
      lor
      if Uchar.of_char '0' <= c && c <= Uchar.of_char '9' then
        Uchar.to_int c - Uchar.to_int (Uchar.of_char '0')
      else if Uchar.of_char 'a' <= c && c <= Uchar.of_char 'f' then
        Uchar.to_int c - Uchar.to_int (Uchar.of_char 'a') + 10
      else
        Uchar.to_int c - Uchar.to_int (Uchar.of_char 'A') + 10
    in
    lit
    |> Uutf.String.fold_utf_8
         (fun (s, i) _ u ->
           let encode c =
             Uutf.encode encoder @@ `Uchar c |> ignore;
             (`Unescaped, i + 1)
           in
           match (s, u) with
           | `Unescaped, `Uchar c ->
             if Uchar.of_char '\\' = c then
               (`Escaped, i + 1)
             else if Uchar.of_char '"' = c then
               (`Unescaped, i + 1)
             else
               encode c
           | `Escaped, `Uchar c ->
             if
               Uchar.of_char '"' = c
               || Uchar.of_char '\\' = c
               || Uchar.of_char '/' = c
             then
               encode c
             else if Uchar.of_char 'b' = c then
               encode (Uchar.of_char '\b')
             else if Uchar.of_char 'f' = c then
               encode (Uchar.of_int 0x0c)
             else if Uchar.of_char 'n' = c then
               encode (Uchar.of_char '\n')
             else if Uchar.of_char 'r' = c then
               encode (Uchar.of_char '\r')
             else if Uchar.of_char 't' = c then
               encode (Uchar.of_char '\t')
             else
               (`Hex0, i + 1)
           | `Hex0, `Uchar c -> (`Hex1 (hex_to_int 0 c), i + 1)
           | `Hex1 h, `Uchar c -> (`Hex2 (hex_to_int h c), i + 1)
           | `Hex2 h, `Uchar c -> (`Hex3 (hex_to_int h c), i + 1)
           | `Hex3 h, `Uchar c -> encode (Uchar.of_int (hex_to_int h c))
           | _, `Malformed _ ->
             failwithf "Malformed UTF-8 in string literal at char index %d" i)
         (`Unescaped, 0)
    |> ignore;
    Uutf.encode encoder `End |> ignore;
    Buffer.contents buffer
end

module Kind = struct
  module Id = Id.Make ()
  module Env = Map.Make (Id)

  type 'k f = [`Star of Loc.t | `Arrow of Loc.t * 'k * 'k | `Var of Loc.t * Id.t]
  type t = [ | t f]

  let at = function `Star at -> at | `Arrow (at, _, _) | `Var (at, _) -> at

  (* *)

  let fresh at = `Var (at, Id.fresh at)

  (* Comparison *)

  let index = function `Star _ -> 0 | `Arrow _ -> 1 | `Var _ -> 2

  let rec compare lhs rhs =
    if lhs == rhs then
      0
    else
      match (lhs, rhs) with
      | `Star _, `Star _ -> 0
      | `Arrow (_, lhs_dom, lhs_cod), `Arrow (_, rhs_dom, rhs_cod) ->
        compare lhs_dom rhs_dom <>? fun () -> compare lhs_cod rhs_cod
      | `Var (_, l), `Var (_, r) -> Id.compare l r
      | _ -> index lhs - index rhs

  (* *)

  let rec min_arity n = function
    | `Star _ -> n
    | `Arrow (_, _, c) -> min_arity (n + 1) c
    | `Var _ -> 1

  let min_arity k = min_arity 0 k

  (* *)

  let freshen env k =
    let rec freshen = function
      | `Star _ as inn -> inn
      | `Arrow (at', d, c) as inn ->
        let d' = freshen d and c' = freshen c in
        if d == d' && c == c' then inn else `Arrow (at', d', c')
      | `Var (at', i) -> (
        match Env.find_opt i !env with
        | None ->
          let v' = fresh at' in
          env := Env.add i v' !env;
          v'
        | Some k -> k)
    in
    freshen k

  (* Formatting *)

  module Numbering = struct
    include Env

    type nonrec t = int t ref

    let create () = ref empty
  end

  let pp ?(numbering = Numbering.create ()) kind =
    let rec pp atomize kind =
      let open FomPP in
      match kind with
      | `Star _ -> star
      | `Arrow (_, dom, cod) ->
        let dom = pp true dom in
        let cod = pp false cod in
        dom ^^ space_arrow_right_break_1 ^^ cod
        |> if atomize then egyptian parens 2 else Fun.id
      | `Var (_, i) ->
        let n =
          match Env.find_opt i !numbering with
          | None ->
            let n = Env.cardinal !numbering in
            numbering := Env.add i n !numbering;
            n
          | Some n -> n
        in
        kappa_lower ^^ subscript n
    in
    pp false kind |> FomPP.group

  let pp_annot ?(numbering = Numbering.create ()) = function
    | `Star _ -> empty
    | kind -> colon ^^ align (pp ~numbering kind)
end

module Label = struct
  include Id.Make ()

  (** If both labels are numeric, comparison is done by numeric value. *)
  let compare lhs rhs =
    if lhs == rhs then
      0
    else
      let lhs = to_string lhs in
      let rhs = to_string rhs in
      try int_of_string lhs - int_of_string rhs
      with Failure _ -> String.compare lhs rhs
end

module Tuple = struct
  let is_tuple labels =
    labels
    |> ListExt.for_alli @@ fun i (l, _) ->
       Label.to_string l = Int.to_string (i + 1)
end

module Typ = struct
  module Const = struct
    type t = [`Bool | `Int | `String]

    (* Comparison *)

    let equal lhs rhs =
      match (lhs, rhs) with
      | `Bool, `Bool | `Int, `Int | `String, `String -> true
      | _ -> false

    let index = function `Bool -> 0 | `Int -> 1 | `String -> 2

    let compare lhs rhs =
      if lhs == rhs then
        0
      else
        match (lhs, rhs) with
        | `Bool, `Bool | `Int, `Int | `String, `String -> 0
        | _ -> index lhs - index rhs

    (* Kinding *)

    let kind_of at t =
      let star = `Star at in
      match t with `Bool | `Int | `String -> star

    (* Formatting *)

    let pp = function `Bool -> bool' | `Int -> int' | `String -> string'
  end

  module Id = Id.Make ()

  type ('t, 'k) f =
    [ `Mu of Loc.t * 't
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * 'k * 't
    | `App of Loc.t * 't * 't
    | `ForAll of Loc.t * 't
    | `Exists of Loc.t * 't
    | `Arrow of Loc.t * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Sum of Loc.t * (Label.t * 't) list ]

  type t = [ | (t, Kind.t) f]

  let at = function
    | `Mu (at, _)
    | `Const (at, _)
    | `Var (at, _)
    | `Lam (at, _, _, _)
    | `App (at, _, _)
    | `ForAll (at, _)
    | `Exists (at, _)
    | `Arrow (at, _, _)
    | `Product (at, _)
    | `Sum (at, _) ->
      at

  let set_at at = function
    | `Mu (_, t) -> `Mu (at, t)
    | `Const (_, c) -> `Const (at, c)
    | `Var (_, i) -> `Var (at, i)
    | `Lam (_, i, k, t) -> `Lam (at, i, k, t)
    | `App (_, f, x) -> `App (at, f, x)
    | `ForAll (_, t) -> `ForAll (at, t)
    | `Exists (_, t) -> `Exists (at, t)
    | `Arrow (_, d, c) -> `Arrow (at, d, c)
    | `Product (_, ls) -> `Product (at, ls)
    | `Sum (_, ls) -> `Sum (at, ls)

  (* Macros *)

  let sort labels = List.sort (Compare.the fst Label.compare) labels
  let product at fs = `Product (at, sort fs)
  let sum at cs = `Sum (at, sort cs)
  let zero at = `Sum (at, [])

  (* Type predicates *)

  let is_int = function `Const (_, `Int) -> true | _ -> false

  (* Type applications *)

  let app at = List.fold_left @@ fun f x -> `App (at, f, x)

  let unapp t =
    let rec loop xs = function
      | `App (_, f, x) -> loop (x :: xs) f
      | f -> (f, xs)
    in
    loop [] t

  let arity_and_result t =
    let rec loop n = function
      | `Arrow (_, _, c) -> loop (n + 1) c
      | r -> (n, r)
    in
    loop 0 t

  (* Substitution *)

  module IdSet = Set.Make (Id)

  let rec free = function
    | `Const _ -> IdSet.empty
    | `Var (_, i) -> IdSet.singleton i
    | `Lam (_, i, _, e) -> free e |> IdSet.remove i
    | `App (_, f, x) -> IdSet.union (free f) (free x)
    | `Mu (_, e) | `ForAll (_, e) | `Exists (_, e) -> free e
    | `Arrow (_, d, c) -> IdSet.union (free d) (free c)
    | `Product (_, ls) | `Sum (_, ls) ->
      ls |> List.fold_left (fun s (_, t) -> IdSet.union s (free t)) IdSet.empty

  module Env = Map.Make (Id)

  let rec subst_rec env = function
    | `Mu (at, t) as inn ->
      let t' = subst_rec env t in
      if t == t' then inn else `Mu (at, t')
    | `Const _ as inn -> inn
    | `Var (_, i) as inn -> (
      match Env.find_opt i env with None -> inn | Some t -> subst_rec env t)
    | `Lam (at, i, k, t) as inn ->
      let env = Env.remove i env in
      if Env.is_empty env then
        inn
      else
        let t' = subst_rec env t in
        if t == t' then inn else `Lam (at, i, k, t')
    | `App (at, f, x) as inn ->
      let f' = subst_rec env f and x' = subst_rec env x in
      if f == f' && x == x' then inn else `App (at, f', x')
    | `ForAll (at, t) as inn ->
      let t' = subst_rec env t in
      if t == t' then inn else `ForAll (at, t')
    | `Exists (at, t) as inn ->
      let t' = subst_rec env t in
      if t == t' then inn else `Exists (at, t')
    | `Arrow (at, d, c) as inn ->
      let d' = subst_rec env d and c' = subst_rec env c in
      if d == d' && c == c' then inn else `Arrow (at, d', c')
    | `Product (at, ls) as inn ->
      let ls' = subst_rec_labeled env ls in
      if ls == ls' then inn else `Product (at, ls')
    | `Sum (at, ls) as inn ->
      let ls' = subst_rec_labeled env ls in
      if ls == ls' then inn else `Sum (at, ls')

  and subst_rec_labeled env ls =
    ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id (subst_rec env))

  let subst_rec env t = if Env.is_empty env then t else subst_rec env t

  let rec is_free id = function
    | `Const _ -> false
    | `Var (_, id') -> Id.equal id id'
    | `Lam (_, id', _, body) -> (not (Id.equal id id')) && is_free id body
    | `App (_, fn, arg) -> is_free id fn || is_free id arg
    | `Mu (_, typ) | `ForAll (_, typ) | `Exists (_, typ) -> is_free id typ
    | `Arrow (_, d, c) -> is_free id d || is_free id c
    | `Product (_, ls) | `Sum (_, ls) ->
      ls |> List.exists @@ fun (_, t) -> is_free id t

  let rec subst_par env = function
    | `Mu (at, t) as inn ->
      let t' = subst_par env t in
      if t == t' then inn else `Mu (at, t')
    | `Const _ as inn -> inn
    | `Var (_, i) as inn -> (
      match Env.find_opt i env with None -> inn | Some t -> t)
    | `Lam (at, i, k, t) as inn ->
      let env = Env.remove i env in
      if Env.is_empty env then
        inn
      else if Env.exists (fun i' t' -> is_free i t' && is_free i' t) env then
        let i' = Id.freshen i in
        let v' = `Var (at, i') in
        let t' = subst_par (Env.add i v' env) t in
        if t == t' then inn else `Lam (at, i', k, t')
      else
        let t' = subst_par env t in
        if t == t' then inn else `Lam (at, i, k, t')
    | `App (at, f, x) as inn ->
      let f' = subst_par env f and x' = subst_par env x in
      if f == f' && x == x' then inn else `App (at, f', x')
    | `ForAll (at, t) as inn ->
      let t' = subst_par env t in
      if t == t' then inn else `ForAll (at, t')
    | `Exists (at, t) as inn ->
      let t' = subst_par env t in
      if t == t' then inn else `Exists (at, t')
    | `Arrow (at, d, c) as inn ->
      let d' = subst_par env d and c' = subst_par env c in
      if d == d' && c == c' then inn else `Arrow (at, d', c')
    | `Product (at, ls) as inn ->
      let ls' = subst_par_labeled env ls in
      if ls == ls' then inn else `Product (at, ls')
    | `Sum (at, ls) as inn ->
      let ls' = subst_par_labeled env ls in
      if ls == ls' then inn else `Sum (at, ls')

  and subst_par_labeled env ls =
    ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id (subst_par env))

  let subst i' t' t = subst_par (Env.add i' t' Env.empty) t
  let subst_par env t = if Env.is_empty env then t else subst_par env t

  let rec norm = function
    | `Mu (at, t) as inn -> (
      match norm t with
      | `Lam (_, i, _, t) when not (is_free i t) -> t
      | t' -> if t == t' then inn else `Mu (at, t'))
    | `Const _ as inn -> inn
    | `Var _ as inn -> inn
    | `Lam (at, i, k, t) as inn -> (
      match norm t with
      | `App (_, f, `Var (_, i')) when Id.equal i i' && not (is_free i f) -> f
      | t' -> if t == t' then inn else `Lam (at, i, k, t'))
    | `App (at, f, x) as inn -> (
      let x' = norm x in
      match norm f with
      | `Lam (_, i, _, t) -> norm (subst i x' t)
      | f' -> if f == f' && x == x' then inn else `App (at, f', x'))
    | `ForAll (at, t) as inn ->
      let t' = norm t in
      if t == t' then inn else `ForAll (at, t')
    | `Exists (at, t) as inn ->
      let t' = norm t in
      if t == t' then inn else `Exists (at, t')
    | `Arrow (at, d, c) as inn ->
      let d' = norm d and c' = norm c in
      if d == d' && c == c' then inn else `Arrow (at, d', c')
    | `Product (at, ls) as inn ->
      let ls' = ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id norm) in
      if ls == ls' then inn else `Product (at, ls')
    | `Sum (at, ls) as inn ->
      let ls' = ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id norm) in
      if ls == ls' then inn else `Sum (at, ls')

  (* Freshening *)

  let freshen t =
    let env = ref Kind.Env.empty in
    let rec freshen = function
      | `Mu (at, t) as inn ->
        let t' = freshen t in
        if t == t' then inn else `Mu (at, t')
      | `Const _ as inn -> inn
      | `Var _ as inn -> inn
      | `Lam (at, i, k, t) as inn ->
        let k' = Kind.freshen env k and t' = freshen t in
        if k == k' && t == t' then inn else `Lam (at, i, k', t')
      | `App (at, f, x) as inn ->
        let f' = freshen f and x' = freshen x in
        if f == f' && x == x' then inn else `App (at, f', x')
      | `ForAll (at, t) as inn ->
        let t' = freshen t in
        if t == t' then inn else `ForAll (at, t')
      | `Exists (at, t) as inn ->
        let t' = freshen t in
        if t == t' then inn else `Exists (at, t')
      | `Arrow (at, d, c) as inn ->
        let d' = freshen d and c' = freshen c in
        if d == d' && c == c' then inn else `Arrow (at, d', c')
      | `Product (at, ls) as inn ->
        let ls' = ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id freshen) in
        if ls == ls' then inn else `Product (at, ls')
      | `Sum (at, ls) as inn ->
        let ls' = ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id freshen) in
        if ls == ls' then inn else `Sum (at, ls')
    in
    freshen t

  (* Comparison *)

  let index = function
    | `Mu _ -> 0
    | `Const _ -> 1
    | `Var _ -> 2
    | `Lam _ -> 3
    | `App _ -> 4
    | `ForAll _ -> 5
    | `Exists _ -> 6
    | `Arrow _ -> 7
    | `Product _ -> 8
    | `Sum _ -> 9

  let rec compare lhs rhs =
    if lhs == rhs then
      0
    else
      match (lhs, rhs) with
      | `Mu (_, lhs), `Mu (_, rhs) -> compare lhs rhs
      | `Const (_, lhs), `Const (_, rhs) -> Const.compare lhs rhs
      | `Var (_, lhs), `Var (_, rhs) -> Id.compare lhs rhs
      | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
        ->
        Kind.compare lhs_kind rhs_kind <>? fun () ->
        if Id.equal lhs_id rhs_id then
          compare lhs_typ rhs_typ
        else
          let v = `Var (Loc.dummy, Id.fresh Loc.dummy) in
          compare (subst lhs_id v lhs_typ) (subst rhs_id v rhs_typ)
      | `App (_, lhs_fn, lhs_arg), `App (_, rhs_fn, rhs_arg) ->
        compare lhs_fn rhs_fn <>? fun () -> compare lhs_arg rhs_arg
      | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs)
        ->
        compare lhs rhs
      | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
        compare lhs_d rhs_d <>? fun () -> compare lhs_c rhs_c
      | `Product (_, lhs_ls), `Product (_, rhs_ls)
      | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
        ListExt.compare_with
          (fun (lhs_l, lhs_t) (rhs_l, rhs_t) ->
            Label.compare lhs_l rhs_l <>? fun () -> compare lhs_t rhs_t)
          lhs_ls rhs_ls
      | _ -> index lhs - index rhs

  (* Formatting *)

  let prec_min = 0
  let prec_arrow = 1
  let prec_app = 2
  let prec_max = 3

  (* *)

  let some_spaces = Some spaces

  let rec hanging = function
    | `Lam _ | `Mu (_, `Lam _) | `ForAll (_, `Lam _) | `Exists (_, `Lam _) ->
      some_spaces
    | `Product _ -> some_spaces
    | `App _ as t -> (
      match unapp t with `Var _, [x] -> hanging x | _ -> None)
    | _ -> None

  let rec binding pp_annot prec_outer head i k t =
    (group (head ^^ Id.pp i ^^ pp_annot k ^^ dot |> nest 2)
    ^^
    match hanging t with
    | Some _ -> pp pp_annot prec_min t
    | None -> break_0 ^^ group (pp pp_annot prec_min t) |> nest 2 |> group)
    |> if prec_min < prec_outer then egyptian parens 2 else Fun.id

  and quantifier pp_annot prec_outer symbol (typ : t) =
    match typ with
    | `Lam (_, id, kind, body) ->
      binding pp_annot prec_outer symbol id kind body
    | _ -> symbol ^^ egyptian parens 2 (pp pp_annot prec_min typ)

  and labeled pp_annot labels =
    labels
    |> List.stable_sort (Compare.the (fst >>> Label.at >>> fst) Pos.compare)
    |> List.map (function
         | l, `Var (_, i) when Id.name i = Label.name l -> Label.pp l
         | label, typ -> (
           Label.pp label ^^ colon
           ^^
           match hanging typ with
           | Some (lhs, _) -> lhs ^^ pp pp_annot prec_min typ
           | None -> break_1 ^^ pp pp_annot prec_min typ |> nest 2 |> group))
    |> separate comma_break_1

  and ticked pp_annot labels =
    match
      labels
      |> List.stable_sort (Compare.the (fst >>> Label.at >>> fst) Pos.compare)
      |> List.map @@ function
         | l, `Product (_, []) -> tick ^^ Label.pp l
         | l, t ->
           tick ^^ Label.pp l ^^ break_1 ^^ pp pp_annot prec_max t
           |> nest 2 |> group
    with
    | [l] -> l
    | [] -> pipe
    | ls ->
      ls |> separate break_1_pipe_space |> precede (ifflat empty pipe_space)

  and tuple pp_annot labels =
    labels |> List.map (snd >>> pp pp_annot prec_min) |> separate comma_break_1

  and pp pp_annot prec_outer (typ : t) =
    match typ with
    | `Const (_, const) -> Const.pp const
    | `Var (_, id) -> Id.pp id
    | `Lam (_, id, kind, body) ->
      binding pp_annot prec_outer lambda_lower id kind body
    | `Mu (_, typ) -> quantifier pp_annot prec_outer FomPP.mu_lower typ
    | `ForAll (_, typ) -> quantifier pp_annot prec_outer FomPP.for_all typ
    | `Exists (_, typ) -> quantifier pp_annot prec_outer FomPP.exists typ
    | `Arrow (_, dom, cod) ->
      pp pp_annot (prec_arrow + 1) dom
      ^^ (match hanging cod with
         | Some (lhs, _) -> space_arrow_right ^^ lhs
         | None -> space_arrow_right_break_1)
      ^^ pp pp_annot (prec_arrow - 1) cod
      |> if prec_arrow < prec_outer then egyptian parens 2 else Fun.id
    | `Product (_, labels) ->
      if Tuple.is_tuple labels then
        tuple pp_annot labels |> egyptian parens 2
      else
        labeled pp_annot labels |> egyptian braces 2
    | `Sum (_, labels) ->
      ticked pp_annot labels
      |> if prec_arrow < prec_outer then egyptian parens 2 else Fun.id
    | `App (_, _, _) -> (
      match unapp typ with
      | f, xs ->
        pp pp_annot prec_app f
        :: (xs |> List.map (pp pp_annot (prec_app + 1) >>> group))
        |> separate break_1
        |> if prec_app < prec_outer then egyptian parens 2 else group)

  let pp ?(pp_annot = Kind.pp_annot ~numbering:(Kind.Numbering.create ())) typ =
    pp pp_annot prec_min typ |> group
end

module Exp = struct
  let bool = `Const (Loc.dummy, `Bool)
  let int = `Const (Loc.dummy, `Int)

  module Const = struct
    type ('nat, 't) t =
      [ `LitBool of bool
      | `LitNat of 'nat
      | `LitString of string
      | `OpArithAdd
      | `OpArithDiv
      | `OpArithMinus
      | `OpArithMul
      | `OpArithPlus
      | `OpArithRem
      | `OpArithSub
      | `OpCmpGt
      | `OpCmpGtEq
      | `OpCmpLt
      | `OpCmpLtEq
      | `OpEq of 't
      | `OpEqNot of 't
      | `OpLogicalAnd
      | `OpLogicalNot
      | `OpLogicalOr ]

    (* Typing *)

    let type_of at = function
      | `LitBool _ -> `Const (at, `Bool)
      | `LitNat _ -> `Const (at, `Int)
      | `LitString _ -> `Const (at, `String)
      | `OpArithAdd | `OpArithSub | `OpArithMul | `OpArithDiv | `OpArithRem ->
        `Arrow (at, int, `Arrow (at, int, int))
      | `OpArithPlus | `OpArithMinus -> `Arrow (at, int, int)
      | `OpCmpLt | `OpCmpLtEq | `OpCmpGt | `OpCmpGtEq ->
        `Arrow (at, int, `Arrow (at, int, bool))
      | `OpEq typ | `OpEqNot typ -> `Arrow (at, typ, `Arrow (at, typ, bool))
      | `OpLogicalAnd | `OpLogicalOr ->
        `Arrow (at, bool, `Arrow (at, bool, bool))
      | `OpLogicalNot -> `Arrow (at, bool, bool)

    (* Substitution *)

    let map_typ tu = function
      | ( `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
        | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
        | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
        | `OpLogicalNot | `OpLogicalOr ) as c ->
        c
      | `OpEq t -> `OpEq (tu t)
      | `OpEqNot t -> `OpEqNot (tu t)

    let traverse_typ tuM =
      let open Rea in
      function
      | ( `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
        | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
        | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
        | `OpLogicalNot | `OpLogicalOr ) as c ->
        return c
      | `OpEq t ->
        let+ t = tuM t in
        `OpEq t
      | `OpEqNot t ->
        let+ t = tuM t in
        `OpEqNot t

    let collect_typ = function
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
      | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
      | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
      | `OpLogicalNot | `OpLogicalOr ->
        []
      | `OpEq t -> [t]
      | `OpEqNot t -> [t]

    let lit_false = `LitBool false
    let lit_true = `LitBool true

    (* Comparison *)

    let index = function
      | `LitBool _ -> 0
      | `LitNat _ -> 1
      | `LitString _ -> 2
      | `OpArithAdd -> 3
      | `OpArithDiv -> 4
      | `OpArithMinus -> 5
      | `OpArithMul -> 6
      | `OpArithPlus -> 7
      | `OpArithRem -> 8
      | `OpArithSub -> 9
      | `OpCmpGt -> 10
      | `OpCmpGtEq -> 11
      | `OpCmpLt -> 12
      | `OpCmpLtEq -> 13
      | `OpEq _ -> 14
      | `OpEqNot _ -> 15
      | `OpLogicalAnd -> 16
      | `OpLogicalNot -> 17
      | `OpLogicalOr -> 18

    let compare' nat typ l r =
      match (l, r) with
      | `LitBool l, `LitBool r -> Bool.compare l r
      | `LitNat l, `LitNat r -> nat l r
      | `LitString l, `LitString r -> LitString.compare l r
      | `OpEq l, `OpEq r | `OpEqNot l, `OpEqNot r -> typ l r
      | `OpArithAdd, `OpArithAdd
      | `OpArithDiv, `OpArithDiv
      | `OpArithMinus, `OpArithMinus
      | `OpArithMul, `OpArithMul
      | `OpArithPlus, `OpArithPlus
      | `OpArithRem, `OpArithRem
      | `OpArithSub, `OpArithSub
      | `OpCmpGt, `OpCmpGt
      | `OpCmpGtEq, `OpCmpGtEq
      | `OpCmpLt, `OpCmpLt
      | `OpCmpLtEq, `OpCmpLtEq
      | `OpLogicalAnd, `OpLogicalAnd
      | `OpLogicalNot, `OpLogicalNot
      | `OpLogicalOr, `OpLogicalOr ->
        0
      | _ -> index l - index r

    (* Formatting *)

    let pp' nat typ = function
      | `LitBool bool -> if bool then true' else false'
      | `LitNat i -> nat i
      | `LitString s -> utf8string s
      | `OpArithAdd -> plus
      | `OpArithDiv -> slash
      | `OpArithMinus -> minus
      | `OpArithMul -> star
      | `OpArithPlus -> plus
      | `OpArithRem -> percent
      | `OpArithSub -> minus
      | `OpCmpGt -> langle
      | `OpCmpGtEq -> greater_equal
      | `OpCmpLt -> rangle
      | `OpCmpLtEq -> less_equal
      | `OpEq t -> equals ^^ egyptian brackets 2 (typ t)
      | `OpEqNot t -> not_equal ^^ egyptian brackets 2 (typ t)
      | `OpLogicalAnd -> logical_and
      | `OpLogicalNot -> logical_not
      | `OpLogicalOr -> logical_or

    let pp = pp' (Bigint.to_string >>> utf8string) Typ.pp
  end

  module Id = Id.Make ()
  module IdSet = Set.Make (Id)
  module Env = Map.Make (Id)

  type ('e, 't, 'k) f =
    [ `Const of Loc.t * (Bigint.t, 't) Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * 't * 'e
    | `App of Loc.t * 'e * 'e
    | `Gen of Loc.t * Typ.Id.t * 'k * 'e
    | `Inst of Loc.t * 'e * 't
    | `LetIn of Loc.t * Id.t * 'e * 'e
    | `Mu of Loc.t * 'e
    | `IfElse of Loc.t * 'e * 'e * 'e
    | `Product of Loc.t * (Label.t * 'e) list
    | `Select of Loc.t * 'e * 'e
    | `Inject of Loc.t * Label.t * 'e
    | `Case of Loc.t * 'e
    | `Pack of Loc.t * 't * 'e * 't
    | `UnpackIn of Loc.t * Typ.Id.t * Id.t * 'e * 'e
    | `Target of Loc.t * 't * LitString.t ]

  type t = [ | (t, Typ.t, Kind.t) f]

  let at = function
    | `Const (at, _)
    | `Var (at, _)
    | `Lam (at, _, _, _)
    | `App (at, _, _)
    | `Gen (at, _, _, _)
    | `Inst (at, _, _)
    | `LetIn (at, _, _, _)
    | `Mu (at, _)
    | `IfElse (at, _, _, _)
    | `Product (at, _)
    | `Select (at, _, _)
    | `Inject (at, _, _)
    | `Case (at, _)
    | `Pack (at, _, _, _)
    | `UnpackIn (at, _, _, _, _)
    | `Target (at, _, _) ->
      at
end
