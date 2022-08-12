open Rea
open StdlibPlus
include FomAST.Row

let check fs =
  fs |> List.map fst |> List.find_dup_opt Label.compare |> function
  | None -> unit
  | Some (l1, l2) -> fail @@ `Error_duplicated_label (Label.at l2, l1)
