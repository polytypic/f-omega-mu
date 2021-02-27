open FomAnnot

(* *)

module Kind = Kind
module Typ = Typ
module Exp = Exp

module Env = struct
  let empty ?(annot : Annot.t = Annot.empty ()) () =
    object
      val exp_env = Exp.Env.empty

      method get_exp_env = exp_env

      method map_exp_env f = {<exp_env = f exp_env>}

      val typ_env = Typ.Env.empty

      method get_typ_env = typ_env

      method map_typ_env f = {<typ_env = f typ_env>}

      method annotations = annot
    end
end
