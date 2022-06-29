open Higher.Syntax
open Functor.Syntax
open Applicative.Syntax
include Stdlib.Option

let map_fr xyF = function
  | None -> return None
  | Some x -> xyF x >>- fun y -> Some y

let map_eq_fr xxF = function
  | None -> return None
  | Some x as some -> xxF x >>- fun x' -> if x == x' then some else Some x'

let iter_fr xuF = function None -> unit | Some x -> xuF x

(* *)

let exists pr = function None -> false | Some x -> pr x
let or_else r = function None -> r () | some -> some
let both f l r = match (l, r) with Some l, Some r -> Some (f l r) | _ -> None

(* *)

include Higher.New'1 (Stdlib.Option) ()

type 'a fr = < f Monad.t ; f Alternative.t > -> ('a, f) app'1

let methods =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map =
      fun xy xF -> inj (map xy (prj xF))

    method return : 'a. ('a, _) Applicative.return = fun x -> inj (Some x)

    method pair : 'a 'b. ('a, 'b, _) Applicative.pair =
      fun xF yF ->
        inj
          (match (prj xF, prj yF) with
          | Some x, Some y -> Some (x, y)
          | _ -> None)

    method bind : 'a 'b. ('a, 'b, _) Monad.bind =
      fun xyF xF -> inj (bind (prj xF) (fun x -> prj (xyF x)))

    method zero : 'a. ('a, _) Alternative.zero = inj None

    method alt : 'a. ('a, _) Alternative.alt =
      fun lA rA ->
        inj
          (match (prj lA, prj rA) with
          | Some l, _ -> Some l
          | _, Some r -> Some r
          | _ -> None)
  end

let run xF = xF methods |> prj
