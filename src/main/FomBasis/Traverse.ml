open Fun.Syntax
open Applicative.Syntax

let to_map map_fr fn = map_fr (fn >>> return) >>> Identity.run
let to_set map_fr = const >>> to_map map_fr

let to_map_constant map_fr m fn =
  map_fr (fn >>> Constant.from) >>> Constant.run m

let to_get map_fr = to_map_constant map_fr Constant.m id

let to_map_constant_lazy map_fr m fn =
  to_map_constant map_fr m (fun x -> lazy (fn x)) >>> Lazy.force

let to_get_opt map_fr = to_map_constant map_fr Constant.option_m Option.some
let to_exists map_fr = to_map_constant_lazy map_fr Constant.or_lm
let to_find_map map_fr = to_map_constant_lazy map_fr Constant.option_lm

let to_collect map_fr =
  to_map_constant map_fr Constant.cat_m Cat.singleton >>> Cat.to_list

let to_map_reduce map_fr plus zero =
  to_map_constant map_fr @@ Constant.of_monoid
  @@ object
       method identity = zero
       method combine = plus
     end

let to_exists_fr map_fr pr = to_collect map_fr >>> List.exists_fr pr
let to_find_map_fr map_fr pr = to_collect map_fr >>> List.find_map_fr pr

let to_iter_fr map_fr ef =
  map_fr (ef >>> Constant.from) >>> Constant.run Constant.unit_fr_m
