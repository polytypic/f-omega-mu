open FomAST
include Lam

module Const = struct
  include Const

  let bi_2_pow_31, bi_2_pow_32, bi_2_pow_32_minus_1 =
    let open Bigint in
    ( shift_left (of_int 1) 31,
      shift_left (of_int 1) 32,
      shift_left (of_int 1) 32 - of_int 1 )

  let erase = function
    | `LitNat nat ->
      let open Bigint in
      let nat = bit_and nat bi_2_pow_32_minus_1 in
      (* TODO: Warn when literal is truncated. *)
      `LitNat
        (Int32.of_string
           (if nat < bi_2_pow_31 then to_string nat
           else nat - bi_2_pow_32 |> to_string))
    | ( `LitBool _ | `LitString _ | `LitUnit | `OpArithAdd | `OpArithDiv
      | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
      | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
      | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr | `OpStringCat | `Keep _
      | `Target _ ) as other ->
      other
end

let rec erase = function
  | `Const (_, c) -> `Const (Const.erase c)
  | `Var (_, i) -> `Var i
  | `Lam (_, i, _, e) -> `Lam (i, erase e)
  | `App (_, f, x) -> `App (erase f, erase x)
  | `UnpackIn (_, _, _, i, v, e) -> `App (`Lam (i, erase e), erase v)
  | `Mu (_, e) -> `Mu (erase e)
  | `IfElse (_, c, t, e) -> `IfElse (erase c, erase t, erase e)
  | `Product (_, fs) -> `Product (fs |> Row.map erase)
  | `Select (_, e, l) -> `Select (erase e, erase l)
  | `Inject (_, l, e) -> `Inject (l, erase e)
  | `Case (_, cs) -> `Case (erase cs)
  | `Gen (_, _, _, e) | `Inst (_, e, _) | `Pack (_, _, e, _) -> erase e
