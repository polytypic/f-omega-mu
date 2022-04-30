include Stdlib.Seq
open Monad.Syntax
open Applicative.Syntax

let rec exists_fr (p : 'a -> _) (xs : 'a t) =
  match uncons xs with
  | Some (x, xs) -> p x ||| exists_fr p xs
  | None -> return false
