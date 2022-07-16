open StdlibPlus
open FomSource

module Kind = struct
  module Unk = Id.Make ()
  module UnkMap = Map.Make (Unk)

  module Core = struct
    type 'k f = [`Star of Loc.t | `Arrow of Loc.t * 'k * 'k]

    let map_at_fr fn = function
      | `Star at -> fn at >>- fun at -> `Star at
      | `Arrow (at, d, c) -> fn at >>- fun at -> `Arrow (at, d, c)
  end

  type 'k f = ['k Core.f | `Unk of Loc.t * Unk.t]
  type t = t f

  module UnkEnv = struct
    type m = t UnkMap.t MVar.t Oo.Prop.t

    let empty () = MVar.create UnkMap.empty
    let field r : m = r#kind_env
    let resetting op = setting field (empty ()) op
    let find_opt i = read field >>- UnkMap.find_opt i
    let add i k = mutate field @@ UnkMap.add i k
    let cloning op = read field >>= fun v -> setting field (MVar.create v) op

    class con =
      object
        val mutable kind_env = empty ()
        method kind_env : m = prop (fun () -> kind_env) (fun x -> kind_env <- x)
      end
  end

  let map_at_fr fn = function
    | #Core.f as t -> Core.map_at_fr fn t
    | `Unk (at, i) -> fn at >>- fun at -> `Unk (at, i)

  let at t = Traverse.to_get map_at_fr t
  let set_at at = Traverse.to_set map_at_fr at

  (* *)

  let fresh at = `Unk (at, Unk.fresh at)

  (* Comparison *)

  let tag = function `Star _ -> `Star | `Arrow _ -> `Arrow | `Unk _ -> `Unk

  let rec compare lhs rhs =
    if lhs == rhs then 0
    else
      match (lhs, rhs) with
      | `Star _, `Star _ -> 0
      | `Arrow (_, lhs_dom, lhs_cod), `Arrow (_, rhs_dom, rhs_cod) ->
        compare lhs_dom rhs_dom <>? fun () -> compare lhs_cod rhs_cod
      | `Unk (_, l), `Unk (_, r) -> Unk.compare l r
      | _ -> Stdlib.compare (tag lhs) (tag rhs)

  (* *)

  let rec min_arity n = function
    | `Star _ | `Unk _ -> n
    | `Arrow (_, _, c) -> min_arity (n + 1) c

  let min_arity k = min_arity 0 k

  (* *)

  let eq l r =
    match (l, r) with
    | `Star l, `Star r -> l == r
    | `Arrow l, `Arrow r -> eq'3 l r
    | `Unk l, `Unk r -> eq'2 l r
    | _ -> false

  (* *)

  let keep_phys_eq' k k' = if k == k' || eq k k' then k else k'
  let keep_phys_eq fn k = keep_phys_eq' k (fn k)
  let keep_phys_eq_fr fn k = fn k >>- keep_phys_eq' k

  (* *)

  let rec freshen env =
    keep_phys_eq @@ function
    | `Star _ as inn -> inn
    | `Arrow (at', d, c) -> `Arrow (at', freshen env d, freshen env c)
    | `Unk (at', i) -> (
      match UnkMap.find_opt i !env with
      | None ->
        let v' = fresh at' in
        env := UnkMap.add i v' !env;
        v'
      | Some k -> k)
end

module Label = struct
  include Id.Make ()

  (** If both labels are numeric, comparison is done by numeric value. *)
  let compare lhs rhs =
    if lhs == rhs then 0
    else
      let lhs = to_string lhs in
      let rhs = to_string rhs in
      try Bigint.compare (Bigint.of_string lhs) (Bigint.of_string rhs)
      with Failure _ -> String.compare lhs rhs

  let mk s = of_string (Loc.of_path s) s
  let begin' = mk "begin"
  let finish' = mk "finish"
  let string' = mk "string"
  let text' = mk "text"
end

module LabelMap = Map.Make (Label)

module Row = struct
  type 't t = (Label.t * 't) list

  let is_tuple labels =
    (match labels with _ :: _ :: _ -> true | _ -> false)
    && labels
       |> List.for_alli @@ fun i (l, _) ->
          Label.to_string l = Int.to_string (i + 1)

  let rec union_fr lhs rhs both ls rs =
    match (ls, rs) with
    | (l, lx) :: ls', (r, rx) :: rs' ->
      let c = Label.compare l r in
      if c < 0 then
        lhs l lx <*> union_fr lhs rhs both ls' rs >>- fun (y, ys) ->
        (l, y) :: ys
      else if c > 0 then
        rhs r rx <*> union_fr lhs rhs both ls rs' >>- fun (y, ys) ->
        (r, y) :: ys
      else
        both l lx rx <*> union_fr lhs rhs both ls' rs' >>- fun (y, ys) ->
        (l, y) :: ys
    | (l, lx) :: ls, [] ->
      lhs l lx <*> union_fr lhs rhs both ls rs >>- fun (y, ys) -> (l, y) :: ys
    | [], (r, rx) :: rs ->
      rhs r rx <*> union_fr lhs rhs both ls rs >>- fun (y, ys) -> (r, y) :: ys
    | [], [] -> return []

  let map fn = List.map (Pair.map id fn)
  let map_phys_eq fn = List.map_phys_eq (Pair.map_phys_eq id fn)
  let map_fr fn = List.map_fr @@ Pair.map_fr return fn
  let map_phys_eq_fr fn = List.map_phys_eq_fr @@ Pair.map_phys_eq_fr return fn
end

module Tuple = struct
  let labels at' =
    List.mapi (fun i t -> (Label.of_string at' (Int.to_string (i + 1)), t))
end

module Typ = struct
  module Const = struct
    type t = [`Bool | `Int | `String | `Unit]

    (* Comparison *)

    let equal lhs rhs =
      match (lhs, rhs) with
      | `Bool, `Bool | `Int, `Int | `String, `String | `Unit, `Unit -> true
      | _ -> false

    let tag = function
      | `Bool -> `Bool
      | `Int -> `Int
      | `String -> `String
      | `Unit -> `Unit

    let compare lhs rhs =
      if lhs == rhs then 0
      else
        match (lhs, rhs) with
        | `Bool, `Bool | `Int, `Int | `String, `String | `Unit, `Unit -> 0
        | _ -> Stdlib.compare (tag lhs) (tag rhs)

    (* Kinding *)

    let kind_of at t =
      let star = `Star at in
      match t with `Bool | `Int | `String | `Unit -> star
  end

  module Var = struct
    include Id.Make ()

    let to_label i = Label.of_name (at i) (name i)
  end

  module VarSet = Set.Make (Var)
  module VarMap = Map.Make (Var)

  (* Macros *)

  let var i = `Var (Var.at i, i)
  let sort labels = List.sort (Compare.the fst Label.compare) labels
  let row at m fs = `Row (at, m, sort fs)
  let product at = row at `Product
  let sum at = row at `Sum
  let unit at = `Const (at, `Unit)
  let atom l = Label.at l |> fun at -> sum at [(l, unit at)]
  let zero at = sum at []

  let tuple at = function
    | [] -> unit at
    | [t] -> t
    | ts -> product at (Tuple.labels at ts)

  module Core = struct
    type ('t, 'k) f =
      [ `Mu of Loc.t * 't
      | `Const of Loc.t * Const.t
      | `Var of Loc.t * Var.t
      | `Lam of Loc.t * Var.t * 'k * 't
      | `App of Loc.t * 't * 't
      | `Arrow of Loc.t * 't * 't
      | `For of Loc.t * [`All | `Unk] * 't
      | `Row of Loc.t * [`Product | `Sum] * 't Row.t ]

    type t = (t, Kind.t) f

    let map_fr' fl row ft = function
      | `Mu (l, t) -> fl l <*> ft t >>- fun x -> `Mu x
      | `Const (l, c) -> fl l >>- fun l -> `Const (l, c)
      | `Var (l, i) -> fl l >>- fun l -> `Var (l, i)
      | `Lam (l, i, k, t) -> fl l <*> ft t >>- fun (l, t) -> `Lam (l, i, k, t)
      | `App (l, f, x) -> tuple'3 (fl l) (ft f) (ft x) >>- fun x -> `App x
      | `For (l, q, t) -> fl l <*> ft t >>- fun (l, t) -> `For (l, q, t)
      | `Arrow (l, d, c) -> tuple'3 (fl l) (ft d) (ft c) >>- fun x -> `Arrow x
      | `Row (l, m, ls) -> fl l <*> row ft ls >>- fun (l, ls) -> `Row (l, m, ls)

    let map_at_fr fl = map_fr' fl (const return) return
    let set_at at = Traverse.to_set map_at_fr at
    let map_fr fn = map_fr' return Row.map_fr fn
    let map fn = Traverse.to_map map_fr fn
    let map_eq_fr fn = map_fr' return Row.map_phys_eq_fr fn
    let map_eq fn = Traverse.to_map map_eq_fr fn
    let exists fn = Traverse.to_exists map_fr fn
    let exists_fr fn = Traverse.to_exists_fr map_fr fn
    let find_map fn = Traverse.to_find_map map_fr fn
    let iter_fr fn = Traverse.to_iter_fr map_fr fn
    let map_reduce fn = Traverse.to_map_reduce map_fr fn

    (* *)

    let eq l r =
      match (l, r) with
      | `Mu l, `Mu r -> eq'2 l r
      | `Const l, `Const r -> eq'2 l r
      | `Var l, `Var r -> eq'2 l r
      | `Lam l, `Lam r -> eq'4 l r
      | `App l, `App r -> eq'3 l r
      | `Arrow l, `Arrow r -> eq'3 l r
      | `For l, `For r -> eq'3 l r
      | `Row l, `Row r -> eq'3 l r
      | _ -> false

    let keep_phys_eq' t t' = if t == t' || eq t t' then t else t'
    let keep_phys_eq fn t = keep_phys_eq' t (fn t)
    let keep_phys_eq_fr fn t = fn t >>- keep_phys_eq' t

    (* *)

    let rec is_free i = function
      | `Var (_, i') -> return @@ Var.equal i i'
      | `Lam (_, i', _, body) ->
        if Var.equal i i' then return false else is_free i body
      | t -> exists_fr (is_free i) t

    let mu_of_norm at = function
      | `Lam (_, i, _, t) as t' -> (
        is_free i t >>- function false -> t | true -> `Mu (at, t'))
      | t' -> return @@ `Mu (at, t')

    let lam_of_norm at i k = function
      | `App (_, f, `Var (_, i')) as t' when Var.equal i i' -> (
        is_free i f >>- function true -> `Lam (at, i, k, t') | false -> f)
      | t' -> return @@ `Lam (at, i, k, t')

    let rec app_of_norm at f' x' =
      match f' with
      | `Lam (_, i, _, t) -> subst_of_norm i x' t
      | f' -> return @@ `App (at, f', x')

    and subst_of_norm i v t =
      let rec subst_of_norm env =
        keep_phys_eq_fr @@ function
        | `Var (_, i) as inn ->
          return (match VarMap.find_opt i env with None -> inn | Some t -> t)
        | `Mu (at, t) -> subst_of_norm env t >>= mu_of_norm at
        | `Lam (at, i, k, t) as inn ->
          let env = VarMap.remove i env in
          if VarMap.is_empty env then return inn
          else
            let* i' =
              i
              |> Var.Unsafe.smallest @@ fun i ->
                 is_free i inn
                 ||| VarMap.exists_fr
                       (fun i' t' -> is_free i t' &&& is_free i' inn)
                       env
            in
            if Var.equal i i' then subst_of_norm env t >>= lam_of_norm at i k
            else
              subst_of_norm (VarMap.add i (var i') env) t
              >>= lam_of_norm at i' k
        | `App (at, f, x) ->
          subst_of_norm env f <*> subst_of_norm env x
          >>= uncurry @@ app_of_norm at
        | t -> map_eq_fr (subst_of_norm env) t
      in
      match v with
      | `Var (_, i') when Var.equal i i' -> return t
      | v -> subst_of_norm (VarMap.singleton i v) t

    let apps_of_norm at = List.fold_left_fr @@ app_of_norm at
  end

  type ('t, 'k) f =
    [('t, 'k) Core.f | `Bop of Loc.t * [`Join | `Meet | `Eq] * 't * 't]

  type t = (t, Kind.t) f

  let map_fr' fl row ft = function
    | #Core.f as t -> Core.map_fr' fl row ft t
    | `Bop (l, o, x, y) ->
      tuple'3 (fl l) (ft x) (ft y) >>- fun (l, x, y) -> `Bop (l, o, x, y)

  let map_at_fr fl = map_fr' fl (const return) return
  let at t = Traverse.to_get_opt map_at_fr t |> Option.get
  let set_at at = Traverse.to_set map_at_fr at

  (* Type predicates *)

  let is_int = function `Const (_, `Int) -> true | _ -> false

  (* Type applications *)

  let unlam t =
    let rec loop iks = function
      | `Lam (_, i, k, t) -> loop ((i, k) :: iks) t
      | t -> (t, iks)
    in
    loop [] t

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

  (* *)

  let map_fr fn = map_fr' return Row.map_fr fn
  let map fn = Traverse.to_map map_fr fn
  let map_eq_fr fn = map_fr' return Row.map_phys_eq_fr fn
  let map_eq fn = Traverse.to_map map_eq_fr fn
  let map_reduce plus = Traverse.to_map_reduce map_fr plus
  let exists fn = Traverse.to_exists map_fr fn
  let exists_fr fn = Traverse.to_exists_fr map_fr fn
  let find_map fn = Traverse.to_find_map map_fr fn
  let find_map_fr fn = Traverse.to_find_map_fr map_fr fn

  (* *)

  let eq l r =
    match (l, r) with `Bop l, `Bop r -> eq'4 l r | l, r -> Core.eq l r

  let keep_phys_eq' t t' = if t == t' || eq t t' then t else t'
  let keep_phys_eq fn t = keep_phys_eq' t (fn t)
  let keep_phys_eq_fr fn t = fn t >>- keep_phys_eq' t

  (* *)

  let impure = Var.of_string (Loc.of_path "impure") "impure"

  let initial_env =
    let star at = `Star at in
    let arrow d c at = `Arrow (at, d at, c at) in
    let alias name const =
      let at = Loc.of_path name in
      (Var.of_string at name, `Typ (`Const (at, const)))
    in
    VarMap.of_list
      [
        (impure, `Kind (arrow star star (Var.at impure)));
        alias "bool" `Bool;
        alias "int" `Int;
        alias "string" `String;
      ]

  (* *)

  let rec subst_rec env =
    keep_phys_eq @@ function
    | `Var (_, i) as inn -> (
      match VarMap.find_opt i env with None -> inn | Some t -> subst_rec env t)
    | `Lam (at, i, k, t) as inn ->
      let env = VarMap.remove i env in
      if VarMap.is_empty env then inn else `Lam (at, i, k, subst_rec env t)
    | t -> map_eq (subst_rec env) t

  let subst_rec env t = if VarMap.is_empty env then t else subst_rec env t

  let rec is_free i = function
    | `Var (_, i') -> return @@ Var.equal i i'
    | `Lam (_, i', _, body) ->
      if Var.equal i i' then return false else is_free i body
    | t -> exists_fr (is_free i) t

  (* Freshening *)

  let freshen (t : t) : t =
    let env = ref Kind.UnkMap.empty in
    let rec freshen t =
      t
      |> keep_phys_eq @@ function
         | `Lam (at, i, k, t) -> `Lam (at, i, Kind.freshen env k, freshen t)
         | t -> map_eq freshen t
    in
    freshen t

  let freshen = Profiling.Counter.wrap'1 "freshen" freshen

  (* Comparison *)

  let tag = function
    | `Mu _ -> `Mu
    | `Const _ -> `Const
    | `Var _ -> `Var
    | `Lam _ -> `Lam
    | `App _ -> `App
    | `For _ -> `For
    | `Arrow _ -> `Arrow
    | `Row _ -> `Row
    | `Bop _ -> `Bop

  let compare' compare l_env r_env l r =
    match (l, r) with
    | `Mu (_, l), `Mu (_, r) -> compare l_env r_env l r
    | `Const (_, l), `Const (_, r) -> Const.compare l r
    | `Var (_, l), `Var (_, r) ->
      let l = VarMap.find_opt l l_env |> Option.value ~default:l in
      let r = VarMap.find_opt r r_env |> Option.value ~default:r in
      Var.compare l r
    | `Lam (_, l_i, l_k, l_t), `Lam (_, r_i, r_k, r_t) ->
      Kind.compare l_k r_k <>? fun () ->
      if Var.equal l_i r_i then
        compare (VarMap.remove l_i l_env) (VarMap.remove r_i r_env) l_t r_t
      else
        let v = Var.fresh Loc.dummy in
        compare (VarMap.add l_i v l_env) (VarMap.add r_i v r_env) l_t r_t
    | `App (_, l_f, l_x), `App (_, r_f, r_x) ->
      compare l_env r_env l_x r_x <>? fun () -> compare l_env r_env l_f r_f
    | `For (_, l_q, l_t), `For (_, r_q, r_t) ->
      Stdlib.compare l_q r_q <>? fun () -> compare l_env r_env l_t r_t
    | `Arrow (_, l_d, l_c), `Arrow (_, r_d, r_c) ->
      compare l_env r_env l_d r_d <>? fun () -> compare l_env r_env l_c r_c
    | `Row (_, l_m, l_ls), `Row (_, r_m, r_ls) ->
      Stdlib.compare l_m r_m <>? fun () ->
      List.compare_with
        (fun (l_l, l_t) (r_l, r_t) ->
          Label.compare l_l r_l <>? fun () -> compare l_env r_env l_t r_t)
        l_ls r_ls
    | `Bop (_, l_o, l_x, l_y), `Bop (_, r_o, r_x, r_y) ->
      Stdlib.compare l_o r_o <>? fun () ->
      compare l_env r_env l_x r_x <>? fun () -> compare l_env r_env l_y r_y
    | _ -> Stdlib.compare (tag l) (tag r)

  let rec compare_in_env l_env r_env l r =
    compare' compare_in_env l_env r_env l r

  let compare =
    Profiling.Counter.wrap'2 "compare"
      (compare_in_env VarMap.empty VarMap.empty)
end

module Exp = struct
  let bool = `Const (Loc.dummy, `Bool)
  let int = `Const (Loc.dummy, `Int)
  let string = `Const (Loc.dummy, `String)
  let impure = `Var (Loc.dummy, Typ.impure)

  module Const = struct
    type ('nat, 't) t =
      [ `Bool of bool
      | `Nat of 'nat
      | `String of JsonString.t
      | `Unit
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
      | `OpLogicalOr
      | `OpStringCat
      | `Keep of 't
      | `Target of 't * JsonString.t ]

    (* Typing *)

    let type_of at =
      let bpr t = `Arrow (at, t, `Arrow (at, t, bool)) in
      let uop t = `Arrow (at, t, t) in
      let bop t = `Arrow (at, t, uop t) in
      function
      | `Bool _ -> `Const (at, `Bool)
      | `Nat _ -> `Const (at, `Int)
      | `String _ -> `Const (at, `String)
      | `Unit -> `Const (at, `Unit)
      | `OpArithAdd | `OpArithSub | `OpArithMul | `OpArithDiv | `OpArithRem ->
        bop int
      | `OpArithPlus | `OpArithMinus -> uop int
      | `OpCmpLt | `OpCmpLtEq | `OpCmpGt | `OpCmpGtEq -> bpr int
      | `OpEq typ | `OpEqNot typ -> bpr typ
      | `OpLogicalAnd | `OpLogicalOr -> bop bool
      | `OpLogicalNot -> uop bool
      | `Keep t -> `Arrow (at, `App (at, impure, t), t)
      | `OpStringCat -> bop string
      | `Target (t, _) -> t

    (* Substitution *)

    let map_typ_fr tuM = function
      | ( `Bool _ | `Nat _ | `String _ | `Unit | `OpArithAdd | `OpArithDiv
        | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
        | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
        | `OpLogicalNot | `OpLogicalOr | `OpStringCat ) as c ->
        return c
      | `OpEq t -> tuM t >>- fun t -> `OpEq t
      | `OpEqNot t -> tuM t >>- fun t -> `OpEqNot t
      | `Keep t -> tuM t >>- fun t -> `Keep t
      | `Target (t, l) -> tuM t >>- fun t -> `Target (t, l)

    (* *)

    let lit_false = `Bool false
    let lit_true = `Bool true

    (* Comparison *)

    let tag = function
      | `Bool _ -> `Bool
      | `Nat _ -> `Nat
      | `String _ -> `String
      | `Unit -> `Unit
      | `OpArithAdd -> `OpAritAdd
      | `OpArithDiv -> `OpArithDiv
      | `OpArithMinus -> `OpArithMinus
      | `OpArithMul -> `OpArithMul
      | `OpArithPlus -> `OpArithPlus
      | `OpArithRem -> `OpArithRem
      | `OpArithSub -> `OpArithSub
      | `OpCmpGt -> `OpCmpGt
      | `OpCmpGtEq -> `OpCmpGtEq
      | `OpCmpLt -> `OpCmpLt
      | `OpCmpLtEq -> `OpCmpLtEq
      | `OpEq _ -> `OpEq
      | `OpEqNot _ -> `OpEqNot
      | `OpLogicalAnd -> `OpLogicalAnd
      | `OpLogicalNot -> `OpLogicalNot
      | `OpLogicalOr -> `OpLogicalOr
      | `OpStringCat -> `OpStringCat
      | `Keep _ -> `Keep
      | `Target _ -> `Target

    let compare' nat typ l r =
      match (l, r) with
      | `Bool l, `Bool r -> Bool.compare l r
      | `Nat l, `Nat r -> nat l r
      | `String l, `String r -> JsonString.compare l r
      | `OpEq l, `OpEq r | `OpEqNot l, `OpEqNot r -> typ l r
      | `Keep tl, `Keep tr -> typ tl tr
      | `Target (tl, ll), `Target (tr, lr) ->
        typ tl tr <>? fun () -> JsonString.compare ll lr
      | _ -> Stdlib.compare (tag l) (tag r)
  end

  module Var = struct
    include Id.Make ()

    let to_label i = Label.of_name (at i) (name i)
    let of_label l = of_name (Label.at l) (Label.name l)
  end

  module VarSet = Set.Make (Var)
  module VarMap = Map.Make (Var)

  module Core = struct
    type ('e, 't, 'k) f =
      [ `Const of Loc.t * (Bigint.t, 't) Const.t
      | `Var of Loc.t * Var.t
      | `Lam of Loc.t * Var.t * 't * 'e
      | `App of Loc.t * 'e * 'e
      | `Gen of Loc.t * Typ.Var.t * 'k * 'e
      | `Inst of Loc.t * 'e * 't
      | `Mu of Loc.t * 'e
      | `IfElse of Loc.t * 'e * 'e * 'e
      | `Product of Loc.t * 'e Row.t
      | `Select of Loc.t * 'e * 'e
      | `Inject of Loc.t * Label.t * 'e
      | `Case of Loc.t * 'e
      | `Pack of Loc.t * 't * 'e * 't
      | `UnpackIn of Loc.t * Typ.Var.t * 'k * Var.t * 'e * 'e ]

    type t = (t, Typ.Core.t, Kind.t) f

    let at = function
      | `Const (at, _)
      | `Var (at, _)
      | `Lam (at, _, _, _)
      | `App (at, _, _)
      | `Gen (at, _, _, _)
      | `Inst (at, _, _)
      | `Mu (at, _)
      | `IfElse (at, _, _, _)
      | `Product (at, _)
      | `Select (at, _, _)
      | `Inject (at, _, _)
      | `Case (at, _)
      | `Pack (at, _, _, _)
      | `UnpackIn (at, _, _, _, _, _)
      | `Target (at, _, _) ->
        at
  end

  type ('e, 't, 'k) f =
    [ ('e, 't, 'k) Core.f
    | `LamImp of Loc.t * Var.t * 'e
    | `PackImp of Loc.t * 't * 'e
    | `Merge of Loc.t * 'e * 'e ]

  type t = (t, Typ.t, Kind.t) f

  let at = function
    | #Core.f as e -> Core.at e
    | `LamImp (at, _, _) | `PackImp (at, _, _) | `Merge (at, _, _) -> at

  (* *)

  let var i = `Var (Var.at i, i)

  let tuple at = function
    | [] -> `Const (at, `Unit)
    | [e] -> e
    | es -> `Product (at, Tuple.labels at es)

  let product at fs = `Product (at, fs)

  let atom l =
    let at = Label.at l in
    `Inject (at, l, `Const (at, `Unit))

  let lit_bool at value =
    `Const (at, if value then Const.lit_true else Const.lit_false)

  (* *)

  let raw = Var.of_string (Loc.of_path "raw") "_"

  let builtins =
    let mk name fn =
      let at = Loc.of_path name in
      (Var.of_string at name, fn at)
    in
    [
      mk "true" (fun at -> `Const (at, `Bool true));
      mk "false" (fun at -> `Const (at, `Bool false));
      mk "keep" (fun at ->
          let t = Typ.Var.of_string (Loc.of_path "α") "α" in
          `Gen (at, t, `Star at, `Const (at, `Keep (Typ.var t))));
      ( raw,
        let at' = Var.at raw in
        let at = Loc.dummy in
        let s = Var.of_string at "s"
        and p = Var.of_string at "p"
        and string = `Const (at, `String) in
        let lam i t e = `Lam (at, i, t, e)
        and app f x = `App (at, f, x)
        and empty = `Const (at, `String (JsonString.of_utf8 "")) in
        let rev_cat =
          lam s string
            (lam p string
               (app (app (`Const (at, `OpStringCat)) (var p)) (var s)))
        in
        `Product
          ( at',
            [
              (Label.begin', empty);
              (Label.finish', lam s string (var s));
              (Label.string', rev_cat);
              (Label.text', rev_cat);
            ] ) );
    ]

  let initial_exp e =
    builtins |> List.rev
    |> List.fold_left
         (fun e (i, v) -> `App (Var.at i, `LamImp (Var.at i, i, e), v))
         e
end
