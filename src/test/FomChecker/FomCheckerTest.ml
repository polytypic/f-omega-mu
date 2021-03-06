open FomBasis
open FomTest
open FomChecker
open FomParser
open FomElab
open FomEnv

(* *)

open Rea

let parse_typ source and_then =
  source
  |> Parser.parse_utf_8 Grammar.typ_exp Lexer.offside
  >>= elaborate_typ
  |> with_env (ignore >>> FomEnv.Env.empty)
  |> try_in and_then @@ fun _ -> verify false

(* *)

let test_typ_parses_as name source check =
  test name @@ fun () -> parse_typ source check

let () =
  test_typ_parses_as "find_opt_non_contractive >> is_none" "μxs.int→xs"
  @@ fun typ ->
  verify (Typ.find_opt_non_contractive Typ.IdSet.empty typ |> Option.is_none)

let () =
  test_typ_parses_as "find_opt_non_contractive >> is_some [A]" "μxs.xs"
  @@ fun typ ->
  verify (Typ.find_opt_non_contractive Typ.IdSet.empty typ |> Option.is_some)

let () =
  test_typ_parses_as "find_opt_non_contractive >> is_some [B]"
    "(μf.λx.λy.f y x) int string"
  @@ fun typ ->
  verify (Typ.find_opt_non_contractive Typ.IdSet.empty typ |> Option.is_some)

(* *)

let test_typs_parse_as name source1 source2 check =
  test name @@ fun () ->
  parse_typ source1 @@ fun typ1 -> parse_typ source2 (check typ1)

let test_equal_typs source1 source2 =
  test_typs_parse_as "Typ.is_equal_of_norm" source1 source2 @@ fun typ1 typ2 ->
  Typ.is_equal_of_norm (Typ.norm typ1, Typ.norm typ2)
  |> with_env (ignore >>> Env.empty)
  >>= verify

let test_not_equal_typs source1 source2 =
  test_typs_parse_as "Typ.is_equal_of_norm" source1 source2 @@ fun typ1 typ2 ->
  Typ.is_equal_of_norm (Typ.norm typ1, Typ.norm typ2)
  |> with_env (ignore >>> Env.empty)
  >>- not >>= verify

let () =
  test_equal_typs "λt.μl.'nil t | 'cons l" "μl.λt.'nil t | 'cons (l t)";
  test_equal_typs "λx.μxs.x→xs" "λy.y→(μys.y→y→ys)";
  test_equal_typs "λx.x" "λy.μys.y";
  test_equal_typs "∀x.x→x" "∀y.y→y";
  test_equal_typs "λf:*→*.f" "λf.λy.(λx.f x) y";
  test_equal_typs "μx.x" "μx.μy.y";
  test_not_equal_typs "∀x.∀y.x→y" "∀y.∀x.x→y";
  test_not_equal_typs "∀x.x→x" "∀y.y→y→y";
  test_not_equal_typs "λx.μxs.x→xs" "λy.y→y"

(* *)

let parse_exp source and_then =
  source
  |> Parser.parse_utf_8 Grammar.program Lexer.offside
  >>= elaborate
  |> with_env (ignore >>> FomEnv.Env.empty)
  |> try_in and_then @@ fun _ -> verify false

let testInfersAs name typ exp =
  test name @@ fun () ->
  parse_typ typ @@ fun expected ->
  let expected = Typ.norm expected in
  parse_exp exp @@ fun (_, actual, _) ->
  let actual = Typ.norm actual in
  Typ.is_equal_of_norm (expected, actual) |> with_env (ignore >>> Env.empty)
  >>= fun are_equal ->
  if not are_equal then (
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
  else
    unit

let () =
  testInfersAs "fact" "int"
    {eof|
    let fact =
      rec fact: int -> int =>
        fun n: int =>
          if n =[int] 0
          then 1
          else n * fact (n - 1)
    fact 5
    |eof};
  testInfersAs "list encoding" "int"
    {eof|
    type list = λt.μlist.∀r.{nil: r, cons: t → list → r} → r
    let nil = Λt.Λr.λc:{nil: r, cons: t → list t → r}.c.nil
    let cons = Λt.λhd:t.λtl:list t.Λr.λc:{nil: r, cons: t → list t → r}.c.cons hd tl
    let fold = Λt.Λr.μfold:(t → r → r) → r → list t → r.λfn:t → r → r.λz:r.λxs:list t.
      xs[r] {nil = z, cons = λx:t.λxs:list t.fold fn (fn x z) xs}
    let pi_digits = cons[int] 3 (cons[int] 1 (cons[int] 4 (cons[int] 1 (nil[int]))))
    fold[int][int] (λx:int.λs:int.x + s) 0 pi_digits
    |eof};
  testInfersAs "generic fold"
    {eof|∀f.(∀a.∀b.(a → b) → f a → f b) → ∀a.(f a → a) → μ(f) → a|eof}
    {eof|
    type Functor = λf.∀a.∀b.(a → b) → (f a → f b)
    Λf.λfmap: Functor f.
      Λa.λalgebra: f a → a.
        μdoFold: μ(f) → a.
          λv: μ(f).
            algebra (fmap[μ(f)][a] doFold v)
    |eof};
  testInfersAs "existential silly" "∃t.{an: t, do: t → t}"
    {eof|
    type doan = ∃t.{do: t → t, an: t}
    let x =《int\{do = λx:int.x+1, an = 1}》: doan
    let《t\r》= x
    《t\{do = r.do, an = r.do r.an}》: doan
    |eof};
  testInfersAs "hungry function" "(μt.int → t) → μt.int → t"
    "λf:μt.int → t.f 1 2 3";
  testInfersAs "stack ADT" "μlist.'nil | 'cons {hd: int, tl: list}"
    {eof|
    type option = λv.'none | 'some v
    type list = λv.μlist.'nil | 'cons {hd: v, tl: list}
    type Stack = ∃t.{
      empty: ∀v.t v
      push: ∀v.v → t v → t v
      pop: ∀v.t v → option {value: v, stack: t v}
    }
    let《stack\S》 =《list\{
      empty = Λv.'nil : list v
      push = Λv.λx:v.λxs:list v.'cons {hd = x, tl = xs} : list v
      pop = Λv.case {
        nil = λ_:{}.
          'none : option {value: v, stack: list v}
        cons = λr:{hd: v, tl: list v}.
          'some {value = r.hd, stack = r.tl} : option {value: v, stack: list v}
      }
    }》: Stack
    let a_stack = S.push[int] 4 (S.push[int] 1 (S.push[int] 3 (S.empty[int])))
    let to_list = Λv.μto_list:stack v → list v.λs:stack v.
      S.pop[v] s ▷ case {
        none = λ_:{}.
          'nil : list v
        some = λr:{value: v, stack: stack v}.
          'cons {hd = r.value, tl = to_list r.stack} : list v
      }
    to_list[int] a_stack
    |eof};
  testInfersAs "target" "string"
    "type str = string in target[str] \"'a string'\"";
  testInfersAs "type in const" "bool" "type t = int in 1 =[t] 2 || 3 !=[t] 4";
  testInfersAs "mutual rec" "()"
    {eof|
    type opt = λt.'none | 'some t
    type μstream = λt.() → opt (t, stream t)
    let μeven: int → stream int =
      λx:int.λ().'some (x, odd (x+1))
    and μodd: int → stream int =
      λx:int.λ().'some (x, even (x+1))
    in ()
    |eof};
  testInfersAs "unions" "{x: int, y: int} → 'x int | 'y int"
    "if true then λ{x:int}.'x x else λ{y:int}.'y y";
  testInfersAs "intersections" "(|) → {}"
    "if true then λx:'x int.{x} else λy:'y int.{y}";
  testInfersAs "trie" "()"
    {eof|
    type opt = λα.'none | 'some α
    type alt = λα.λβ.'In1 α | 'In2 β
    type μTrie = λκ.λν.∀ρ.Cases ρ → ρ κ ν
    and μCases = λρ.{
      Unit: ∀ν.                        opt ν → ρ ()          ν
      Alt : ∀ν.∀κ1.∀κ2.Trie κ1 ν → Trie κ2 ν → ρ (alt κ1 κ2) ν
      Pair: ∀ν.∀κ1.∀κ2.  Trie κ1 (Trie κ2 ν) → ρ (κ1, κ2)    ν
    }
    let Unit = Λν.        λv:opt ν.                   Λr.λcs:Cases r.cs.Unit[ν] v
    let Alt  = Λν.Λκ1.Λκ2.λt1:Trie κ1 ν.λt2:Trie κ2 ν.Λr.λcs:Cases r.cs.Alt[ν][κ1][κ2] t1 t2
    let Pair = Λν.Λκ1.Λκ2.λt:Trie κ1 (Trie κ2 ν).     Λr.λcs:Cases r.cs.Pair[ν][κ1][κ2] t
    let match = Λρ.λcs:Cases ρ.Λκ.Λν.λt:Trie κ ν.t[ρ] cs
    let μlookup:∀κ.∀ν.Trie κ ν → κ → opt ν = match[λκ.λν.κ → opt ν] {
      Unit = Λν.λv:opt ν.λ().v
      Alt  = Λν.Λκ1.Λκ2.λt1:Trie κ1 ν.λt2:Trie κ2 ν.case {
          In1 = λk1:κ1.lookup[κ1][ν] t1 k1
          In2 = λk2:κ2.lookup[κ2][ν] t2 k2
        }
      Pair = Λν.Λκ1.Λκ2.λt:Trie κ1 (Trie κ2 ν).λ(k1:κ1, k2:κ2).
        lookup[κ1][Trie κ2 ν] t k1 ▷ case {
          none = λ().'none
          some = λt:Trie κ2 ν.lookup[κ2][ν] t k2
        }
    }
    ()
    |eof};
  testInfersAs "fix via μ type" "int"
    {eof|
    type t = λa.λb.μt.t → a → b
    let fix = Λa.Λb.λf:(a → b) → a → b.(λg:t a b.g g) λx:t a b.λn:a.f (x x) n
    let fact = λfact:int → int.λn:int.if n =[int] 0 then 1 else n*fact (n-1)
    fix[int][int] fact 5
    |eof}

let testErrors name exp =
  test name @@ fun () ->
  exp
  |> Parser.parse_utf_8 Grammar.program Lexer.offside
  >>= elaborate
  |> with_env (ignore >>> FomEnv.Env.empty)
  |> try_in
       (fun (_, unexpected, _) ->
         let open FomPP in
         [
           utf8string "Expected type checking to fail, but got type";
           [break_1; Typ.pp unexpected] |> concat |> nest 2;
         ]
         |> concat |> group |> to_string ~max_width:80 |> Printf.eprintf "%s\n";
         verify false)
       (fun _ -> unit)

let () =
  testErrors "non contractive case"
    {eof|
    type μnon_contractive = λt.non_contractive t
    λx:non_contractive int.x ▷ case {}
    |eof};
  testErrors "free variable in def and Λ"
    "type def = λt.x in Λx.λ_:def int.λ_:def string.()";
  testErrors "free variable in def and 《》"
    {eof|
    type r = λt.x
    let《x\_》= 《()\()》: ∃t.t
    (λ_:r int.λ_:r string.(), 1).2
    |eof};
  testErrors "free variable in def and 《》 inside pattern"
    {eof|
    type r = λt.x
    let (《x\_》, _)= (《()\()》: ∃t.t, 101)
    (λ_:r int.λ_:r string.(), 1).2
     |eof};
  testErrors "kind error with type"
    {eof|
    type Apply = λf:(_ → _) → _.λx.f x
    type x = Apply (λx.x → int) int in ()
    |eof};
  testErrors "kind error with type μ"
    {eof|
    type Apply = λf:(_ → _) → _.λx.f x
    type x = Apply (λx.x → int) int in ()
    |eof}
