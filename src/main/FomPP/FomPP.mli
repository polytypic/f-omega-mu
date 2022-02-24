open FomPPrint

module Kind : sig
  module Numbering : sig
    type t

    val create : unit -> t
  end

  val pp : ?numbering:Numbering.t -> FomAST.Kind.t -> document
  val pp_annot : ?numbering:Numbering.t -> FomAST.Kind.t -> document
  val to_string : ?numbering:Numbering.t -> FomAST.Kind.t -> string
end

module Label : sig
  val pp : FomAST.Label.t -> document
end

module Typ : sig
  module Const : sig
    val pp : FomAST.Typ.Const.t -> document
  end

  module Var : sig
    val pp : ?hr:bool -> FomAST.Typ.Var.t -> document
  end

  val hanging :
    ([> ('t, 'k) FomAST.Typ.Core.f] as 't) -> (document * document) option

  val pp :
    ?hr:bool ->
    ?pp_annot:(FomAST.Kind.t -> document) ->
    ([< ('t, FomAST.Kind.t) FomAST.Typ.f > `App
     `Const
     `Exists
     `ForAll
     `Lam
     `Mu
     `Product
     `Sum
     `Var ]
     as
     't) ->
    document

  val to_string :
    ([< ('t, FomAST.Kind.t) FomAST.Typ.f > `App
     `Const
     `Exists
     `ForAll
     `Lam
     `Mu
     `Product
     `Sum
     `Var ]
     as
     't) ->
    string
end

module Exp : sig
  module Var : sig
    val pp : ?hr:bool -> FomAST.Exp.Var.t -> document
  end

  module Const : sig
    val pp' :
      ('nat -> document) ->
      ('t -> document) ->
      ('nat, 't) FomAST.Exp.Const.t ->
      document

    val pp : (Bigint.t, FomAST.Typ.t) FomAST.Exp.Const.t -> document
  end
end
