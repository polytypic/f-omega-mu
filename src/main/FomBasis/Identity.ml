type 'a t = 'a

include
  Higher.New'1
    (struct
      type 'a t = 'a
    end)
    ()

let methods =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map = fun xy xF -> inj (xy (prj xF))
    method return : 'a. ('a, _) Applicative.return = inj

    method pair : 'a 'b. ('a, 'b, _) Applicative.pair =
      fun xF yF -> inj (prj xF, prj yF)

    method bind : 'a 'b. ('a, 'b, _) Monad.bind =
      fun xyF xF -> inj (prj (xyF (prj xF)))
  end

let run xF = xF methods |> prj
