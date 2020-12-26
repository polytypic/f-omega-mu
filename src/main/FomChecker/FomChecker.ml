open FomSource

(* *)

module Kind = Kind
module Typ = Typ
module Exp = Exp

module Env = struct
  let empty
      ?(annotations :
          ( Loc.t,
            < annot :
                [ `Label of Label.t * Typ.t
                | `ExpId of Exp.Id.t * Typ.t
                | `TypId of Typ.Id.t * Kind.t ]
            ; def : Loc.t
            ; uses : Loc.t list ref > )
          Hashtbl.t =
        Hashtbl.create ~random:true 1000) () =
    object
      val exp_env = Exp.Env.empty

      method get_exp_env = exp_env

      method map_exp_env f = {<exp_env = f exp_env>}

      val typ_env = Typ.Env.empty

      method get_typ_env = typ_env

      method map_typ_env f = {<typ_env = f typ_env>}

      method annotations = annotations
    end
end
