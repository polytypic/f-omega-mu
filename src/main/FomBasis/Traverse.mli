val to_map :
  (('a -> (Identity.f, 'b) Monad.frm) -> 's -> (Identity.f, 't) Monad.frm) ->
  ('a -> 'b) ->
  's ->
  't

val to_set :
  (('a -> (Identity.f, 'b) Monad.frm) -> 's -> (Identity.f, 't) Monad.frm) ->
  'b ->
  's ->
  't

val to_map_constant :
  (('a -> ('M -> ('c, 'I) Constant.f'2 as 'R)) -> 's -> 'R) ->
  'M ->
  ('a -> 'c) ->
  's ->
  'c

val to_get :
  (('a -> (('a Constant.f'1, 'I) Functor.frm as 'R)) -> 's -> 'R) -> 's -> 'a

val to_get_opt :
  (('a -> (('a Option.t Constant.f'1, 'I) Applicative.frm as 'R)) -> 's -> 'R) ->
  's ->
  'a Option.t

val to_exists :
  (('a -> ((bool Lazy.t Constant.f'1, 'I) Applicative.frm as 'R)) -> 's -> 'R) ->
  ('a -> bool) ->
  's ->
  bool

val to_exists_fr :
  (('a -> (('a Cat.t Constant.f'1, 'I) Applicative.frm as 'R)) -> 's -> 'R) ->
  ('a -> ('f, 'F, bool) Monad.fr) ->
  's ->
  ('f, 'F, bool) Monad.fr

val to_iter_fr :
  (('a -> ((('f, 'F, unit) Monad.fr Constant.f'1, 'I) Applicative.frm as 'R)) ->
  's ->
  'R) ->
  ('a -> ('f, 'F, unit) Monad.fr) ->
  's ->
  ('f, 'F, unit) Monad.fr

val to_find_map :
  (('a -> (('b Option.t Lazy.t Constant.f'1, 'I) Applicative.frm as 'R)) ->
  's ->
  'R) ->
  ('a -> 'b Option.t) ->
  's ->
  'b Option.t

val to_find_map_fr :
  (('a -> (('a Cat.t Constant.f'1, 'I) Applicative.frm as 'R)) -> 's -> 'R) ->
  ('a -> ('f, 'F, 'b Option.t) Monad.fr) ->
  's ->
  ('f, 'F, 'b Option.t) Monad.fr

val to_collect :
  (('a -> (('a Cat.t Constant.f'1, 'I) Applicative.frm as 'R)) -> 's -> 'R) ->
  's ->
  'a list

val to_map_reduce :
  (('a -> (('m Constant.f'1, 'I) Applicative.frm as 'R)) -> 's -> 'R) ->
  ('m -> 'm -> 'm) ->
  'm ->
  ('a -> 'm) ->
  's ->
  'm
