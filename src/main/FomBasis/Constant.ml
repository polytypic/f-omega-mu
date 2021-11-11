open Higher.Syntax
open Applicative.Syntax

type ('c, 'a) t

external from : 'c -> ('c, 'a) t = "%identity"
external eval : ('c, 'a) t -> 'c = "%identity"

include
  Higher.New'2
    (struct
      type nonrec ('c, 'a) t = ('c, 'a) t
    end)
    ()

let from x = inj @@ from x
let eval x = eval @@ prj x

(* *)

let inj'0 x _ = from x
let inj'1 xy x = inj'0 @@ xy x

(* *)

let ( let+ ) _ xF = from @@ eval xF
let pair combine xF yF = from (combine (eval xF) (eval yF))

let methods =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
  end

let of_monoid m =
  let identity = m#identity and combine = m#combine in
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = inj'0 identity
    method pair : 'a 'b. ('a, 'b, _) Applicative.pair = pair combine
  end

(* *)

let or_lm =
  of_monoid
  @@ object
       method identity = lazy false

       method combine l r =
         if Lazy.is_val l then
           if Lazy.force_val l then l else r
         else if Lazy.is_val r then
           if Lazy.force_val r then r else l
         else
           lazy (Lazy.force l || Lazy.force r)
     end

let option_lm =
  let identity = lazy None
  and combine l r =
    if Lazy.is_val l then
      match Lazy.force_val l with None -> r | _ -> l
    else if Lazy.is_val r then
      match Lazy.force_val r with None -> l | _ -> r
    else
      lazy (match Lazy.force l with None -> Lazy.force r | some -> some)
  in
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = inj'0 identity
    method pair : 'a 'b. ('a, 'b, _) Applicative.pair = pair combine
  end
