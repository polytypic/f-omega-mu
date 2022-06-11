open Higher.Syntax
open Applicative.Syntax
open Monad.Syntax

type ('c, 'a) t

external inject : 'c -> ('c, 'a) t = "%identity"
external project : ('c, 'a) t -> 'c = "%identity"

include
  Higher.New'2
    (struct
      type nonrec ('c, 'a) t = ('c, 'a) t
    end)
    ()

let inject x = inj @@ inject x
let project x = project @@ prj x

type 'c f'1 = ('c, f) app'1
type ('c, 'a) f'2 = ('c, 'a, f) app'2

(* *)

let run m xF = xF m |> project

(* *)

let ( let+ ) _ xF = inject @@ project xF

let m =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
  end

let from x _ = inject x

(* *)

let pair combine xF yF = inject (combine (project xF) (project yF))

let of_monoid m =
  let identity = m#identity and combine = m#combine in
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = from identity
    method pair : 'a 'b. ('a, 'b, _) Applicative.pair = pair combine
  end

(* *)

let cat_m =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = from Cat.empty
    method pair : 'a 'b. ('a, 'b, _) Applicative.pair = pair Cat.append
  end

let or_lm =
  of_monoid
  @@ object
       method identity = lazy false

       method combine l r =
         if Lazy.is_val l then if Lazy.force_val l then l else r
         else if Lazy.is_val r then if Lazy.force_val r then r else l
         else lazy (Lazy.force l || Lazy.force r)
     end

let option_lm =
  let identity = lazy None
  and combine l r =
    if Lazy.is_val l then match Lazy.force_val l with None -> r | _ -> l
    else if Lazy.is_val r then match Lazy.force_val r with None -> l | _ -> r
    else lazy (match Lazy.force l with None -> Lazy.force r | some -> some)
  in
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = from identity
    method pair : 'a 'b. ('a, 'b, _) Applicative.pair = pair combine
  end

let option_m =
  let identity = None and combine l r = match l with None -> r | _ -> l in
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = from identity
    method pair : 'a 'b. ('a, 'b, _) Applicative.pair = pair combine
  end

let unit_fr_m =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = ( let+ )
    method return : 'a. ('a, _) Applicative.return = from unit

    method pair : 'a 'b. ('a, 'b, _) Applicative.pair =
      pair (fun l r -> if r == unit then l else if l == unit then r else l >> r)
  end
