open FomAnnot
open FomCST

module Env = struct
  let empty ?(annot : Annot.t = Annot.empty ()) () =
    object
      method annotations = annot
      val exp_env = Exp.Env.empty
      method get_exp_env = exp_env
      method map_exp_env f = {<exp_env = f exp_env>}
      val typ_env = Typ.Env.empty
      method get_typ_env = typ_env
      method map_typ_env f = {<typ_env = f typ_env>}
      val typ_aliases = Typ.Env.empty
      method get_typ_aliases = typ_aliases
      method map_typ_aliases f = {<typ_aliases = f typ_aliases>}
      val includes = Typ.IncludeMap.empty
      method get_includes = includes
      method map_includes f = {<includes = f includes>}
      val imports = Exp.ImportMap.empty
      method get_imports = imports
      method map_imports f = {<imports = f imports>}
    end
end
