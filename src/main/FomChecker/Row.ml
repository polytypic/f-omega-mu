open FomBasis
include FomAST.Row
open Rea

let check fs =
  let rec check_dups = function
    | l1 :: (l2 :: _ as ls) ->
      if Label.equal l1 l2 then
        fail @@ `Error_duplicated_label (Label.at l2, l1)
      else
        check_dups ls
    | _ -> unit
  in
  fs |> List.map fst |> List.stable_sort Label.compare |> check_dups
