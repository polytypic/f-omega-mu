open FomBasis
open FomSource
open FomTest
open FomChecker
open FomParser
open FomElab

let parse_typ = parse_utf_8 Grammar.typ_exp Lexer.plain
let parse_exp = parse_utf_8 Grammar.program Lexer.plain

let () =
  test "Typ.is_contractive" @@ fun () ->
  verify (Typ.is_contractive (parse_typ "μxs.x→xs"))

let () =
  test "not Typ.is_contractive" @@ fun () ->
  verify (not (Typ.is_contractive (parse_typ "μxs.xs")));
  verify
    (not (Typ.is_contractive (parse_typ "(μf:*→*→*.λx.λy.f y x) a b")))

let () =
  test "Typ.equal_of_norm" @@ fun () ->
  let eq t1 t2 =
    Typ.equal_of_norm (parse_typ t1 |> Typ.norm) (parse_typ t2 |> Typ.norm)
  in
  verify (eq "λt.μl.[nil:t,cons:l]" "μl:*→*.λt.[nil:t,cons:l t]");
  verify (eq "λx.μxs.x→xs" "λy.y→(μys.y→y→ys)");
  verify (eq "λx.x" "λy.μys.y");
  verify (eq "∀x.x→x" "∀y.y→y");
  verify (eq "λf:*→*.f" "λf:*→*.λy.(λx.f x) y");
  verify (eq "μx.x" "μx.μy.y");
  verify (not (eq "∀x.∀y.x→y" "∀y.∀x.x→y"));
  verify (not (eq "∀x.x→x" "∀y.y→y→y"));
  verify (not (eq "λx.μxs.x→xs" "λy.y→y"))

let testChecksAs name typ exp =
  test name @@ fun () ->
  try
    let expected = parse_typ typ |> Typ.norm in
    let env = Env.empty () in
    let actual =
      exp |> parse_exp |> elaborate |> Reader.run env |> Exp.check
      |> Reader.run env
    in
    if not (Typ.equal_of_norm expected actual) then (
      let open FomPP in
      [
        utf8string "Types not equal";
        [break_1; Typ.pp expected] |> concat |> nest 2;
        break_1;
        utf8string "vs";
        [break_1; Typ.pp actual] |> concat |> nest 2;
      ]
      |> concat |> group |> to_string ~max_width:80 |> Printf.eprintf "%s\n";
      verify false)
  with Diagnostic.Error ((_, msg), _) ->
    failwith (msg |> FomPP.to_string ~max_width:80)

let () =
  testChecksAs "fact" "int"
    {eof|
    let fact =
      rec fact: int -> int =>
        fun n: int =>
          if n =[int] 0
          then 1
          else n * fact (n - 1) in
    fact 5
    |eof};
  testChecksAs "list encoding" "int"
    {eof|
    let type list = λt.μlist.∀r.{nil: r, cons: t → list → r} → r in
    let nil = Λt.Λr.λc:{nil: r, cons: t → list t → r}.c.nil in
    let cons = Λt.λhd:t.λtl:list t.Λr.λc:{nil: r, cons: t → list t → r}.c.cons hd tl in
    let fold = Λt.Λr.μfold:(t → r → r) → r → list t → r.λfn:t → r → r.λz:r.λxs:list t.
      xs[r] {nil = z, cons = λx:t.λxs:list t.fold fn (fn x z) xs} in
    let pi_digits = cons[int] 3 (cons[int] 1 (cons[int] 4 (cons[int] 1 (nil[int])))) in
    fold[int][int] (λx:int.λs:int.x + s) 0 pi_digits
    |eof};
  testChecksAs "generic fold"
    {eof|∀f:*→*.(∀a.∀b.(a → b) → f a → f b) → ∀a.(f a → a) → μ(f) → a|eof}
    {eof|
    let type Functor = λf:*→*.∀a.∀b.(a → b) → (f a → f b) in
    Λf:*→*.λfmap: Functor f.
      Λa.λalgebra: f a → a.
        μdoFold: μ(f) → a.
          λv: μ(f).
            algebra (fmap[μ(f)][a] doFold v)
    |eof};
  testChecksAs "existential silly" "∃t.{an: t, do: t → t}"
    {eof|
    let type doan = ∃t.{do: t → t, an: t} in
    let x =《{do = λx:int.x+1, an = 1} : doan/int》in
    let《r / t》= x in
    《{do = r.do, an = r.do r.an} : doan/t》
    |eof};
  testChecksAs "hungry function" "(μt.int → t) → μt.int → t"
    "λf:μt.int → t.f 1 2 3";
  testChecksAs "stack ADT" "μlist.[nil: {}, cons: {hd: int, tl: list}]"
    {eof|
    let type option = λv.[none: {}, some: v] in
    let type list = λv.μlist.[nil: {}, cons: {hd: v, tl: list}] in
    let type Stack = ∃t:* → *.{
      empty: ∀v.t v,
      push: ∀v.v → t v → t v,
      pop: ∀v.t v → option {value: v, stack: t v}
    } in
    let《S/stack》 =《{
      empty = Λv.[nil = {} : list v],
      push = Λv.λx:v.λxs:list v.[cons = {hd = x, tl = xs} : list v],
      pop = Λv.λxs:list v.xs case {
        nil = λ_:{}.
          [none = {} : option {value: v, stack: list v}],
        cons = λr:{hd: v, tl: list v}.
          [some = {value = r.hd, stack = r.tl} : option {value: v, stack: list v}]
      }
    }: Stack/list》in
    let a_stack = S.push[int] 4 (S.push[int] 1 (S.push[int] 3 (S.empty[int]))) in
    let to_list = Λv.μto_list:stack v → list v.λs:stack v.
      S.pop[v] s case {
        none = λ_:{}.
          [nil = {} : list v],
        some = λr:{value: v, stack: stack v}.
          [cons = {hd = r.value, tl = to_list r.stack} : list v]
      } in
    to_list[int] a_stack
    |eof};
  testChecksAs "target" "string" "target[string] \"'a string'\"";
  testChecksAs "let type in const" "bool"
    "let type t = int in 1 =[t] 2 || 3 !=[t] 4"
