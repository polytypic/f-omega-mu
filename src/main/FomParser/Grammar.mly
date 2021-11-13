%token <Bigint.t> LitNat

%token TstrStrPart

%token TstrOpenRaw
%token <string> TstrOpen
%token <FomBasis.JsonString.t> TstrStr
%token <string> TstrEsc
%token TstrClose

%token <string> Id
%token <string> IdDollar
%token <string> IdSub
%token <string> IdTyp

%token <string> Escape

%token <string> Comment

%token And "and"
%token Case "case"
%token Else "else"
%token If "if"
%token Import "import"
%token In "in"
%token Include "include"
%token Let "let"
%token Local "local"
%token Target "target"
%token Then "then"
%token Type "type"

%token ArrowRight "→"
%token BraceLhs "{"
%token BraceRhs "}"
%token BracketLhs "["
%token BracketLhsNS "_["
%token BracketRhs "]"
%token Caret "^"
%token Colon ":"
%token Comma ","
%token Diamond "◇"
%token Dot "."
%token DoubleAngleQuoteLhs "«"
%token DoubleAngleQuoteRhs "»"
%token DoubleComma "„"
%token Equal "="
%token Exists "∃"
%token ForAll "∀"
%token Greater ">"
%token GreaterEqual "≥"
%token LambdaLower "λ"
%token LambdaUpper "Λ"
%token Less "<"
%token LessEqual "≤"
%token LogicalAnd "∧"
%token LogicalNot "¬"
%token LogicalOr "∨"
%token Minus "-"
%token MuLower "μ"
%token NotEqual "≠"
%token ParenLhs "("
%token ParenLhsNS "_("
%token ParenRhs ")"
%token Percent "%"
%token Pipe "|"
%token Plus "+"
%token Slash "/"
%token Star "*"
%token Tick "'"
%token TriangleLhs "◁"
%token TriangleRhs "▷"
%token Underscore "_"

%token EOF

%right "◁"
%left "▷"
%left "◇"
%left "∨"
%left "∧"
%nonassoc "=" "≠" "]"
%nonassoc "<" "≤" "≥" ">"
%left "„"
%left "+" "-" "^"
%left "*" "/" "%"

%start <Exp.t> mods
%start <Typ.t> sigs
%start <Typ.t Typ.Defs.f> incs

%{ open FomCST %}

%%

rev_list_1(elem, sep):
  | e=elem                                              {[e]}
  | es=rev_list_1(elem, sep) sep e=elem                 {e::es}

list_1(elem, sep):
  | es=rev_list_1(elem, sep)                            {List.rev es}

list_n(elem, sep):
  | option(sep)                                         {[]}
  | es=rev_list_1(elem, sep) option(sep)                {List.rev es}

between(lhs, elem, rhs):
  | lhs x=elem rhs                                      {fun f -> f $loc x}

//

kind_atom:
  | "_"                                                 {Kind.fresh $loc}
  | "*"                                                 {`Star $loc}
  | "(" k=kind ")"                                      {k}

kind:
  | k=kind_atom                                         {k}
  | d=kind_atom "→" c=kind                              {`Arrow ($loc, d, c)}

//

label:
  | i=Id                                                {Label.of_string $loc i}
  | n=LitNat                                            {Label.of_string $loc (Bigint.to_string n)}

lab_list(item):
  | ls=list_n(item, ",")                                {ls}

//

lit_string:
  | TstrOpenRaw s=TstrStr TstrClose                     {s}

//

typ_def:
  | "type" bs=list_1(typ_par_def, "and")                {`TypPar ($loc, bs)}
  | "type" bs=list_1(typ_mu_def, "and")                 {`TypRec ($loc, bs)}
  | "include" p=lit_string                              {`Include ($loc, p)}

typ_mu_def:
  | "μ" b=typ_bind "=" t=typ                            {(fst b, snd b, t)}

typ_par_def:
  | b=typ_bind "=" t=typ                                {(fst b, snd b, t)}

//

typ_defs:
  | d=typ_def                                           {d :> _ Typ.Defs.f}
  | d=typ_def "in" ds=typ_defs                          {`In ($loc, d, ds)}
  | "local" d=typ_def "in" ds=typ_defs                  {`LocalIn ($loc, d, ds)}

//

lab_typ:
  | l=label ":" t=typ                                   {(l, t)}
  | i=typ_rid                                           {(Typ.Var.to_label i, Typ.var i)}

tick_lab_typ:
  | "'" l=label                                         {(l, Typ.product $loc [])}
  | "'" l=label t=lab_typ_atom                          {(l, t)}

typ_rid:
  | i=Id                                                {Typ.Var.of_string $loc i}
  | i=IdTyp                                             {Typ.Var.of_string $loc i}

typ_bid:
  | "_"                                                 {Typ.Var.underscore $loc}
  | i=typ_rid                                           {i}

typ_bind:
  | i=typ_bid                                           {(i, Kind.fresh $loc)}
  | i=typ_bid ":" k=kind                                {(i, k)}

typ_atom:
  | i=typ_rid                                           {Typ.var i}
  | "(" ts=list_n(typ, ",") ")"                         {Typ.tuple $loc ts}
  | "{" fs=lab_list(lab_typ) "}"                        {Typ.product $loc fs}
  | "μ" "(" t=typ ")"                                   {`Mu ($loc, t)}
  | "∃" "(" t=typ ")"                                   {`Exists ($loc, t)}
  | "∀" "(" t=typ ")"                                   {`ForAll ($loc, t)}
  | "import" p=lit_string                               {`Import ($loc, p)}

lab_typ_atom:
  | t=typ_atom                                          {t}
  | "'" l=label                                         {Typ.atom l}

typ_app:
  | t=typ_atom                                          {t}
  | f=typ_app x=lab_typ_atom                            {`App ($loc, f, x)}

typ_inf:
  | option("|") s=list_1(tick_lab_typ, "|")             {Typ.sum $loc s}
  | "|"                                                 {Typ.sum $loc []}
  | t=typ_app                                           {t}
  | l=typ_inf "∨" r=typ_inf                             {`Join ($loc, l, r)}
  | l=typ_inf "∧" r=typ_inf                             {`Meet ($loc, l, r)}

typ_arr:
  | t=typ_inf                                           {t}
  | d=typ_inf "→" c=typ                                 {`Arrow ($loc, d, c)}

typ_lam(head):
  | head b=typ_bind "." t=typ                           {`Lam ($loc, fst b, snd b, t)}

typ:
  | t=typ_arr                                           {t}
  | t=typ_lam("μ")                                      {`Mu ($loc, t)}
  | t=typ_lam("∃")                                      {`Exists ($loc, t)}
  | t=typ_lam("∀")                                      {`ForAll ($loc, t)}
  | t=typ_lam("λ")                                      {t}
  | d=typ_def "in" t=typ                                {`LetDefIn ($loc, d, t)}

//

annot_let:
  |                                                     {Typ.zero $loc}

annot_lam:
  | ":" t=typ                                           {t}

lab_pat(annot):
  | l=label "=" p=pat(annot)                            {(l, `Pat p)}
  | l=label a=annot                                     {(l, `Ann a)}

pat(annot):
  | i=exp_bid a=annot                                   {`Id ($loc, i, a)}
  | "(" ps=list_n(pat(annot), ",") ")"                  {Exp.Pat.tuple $loc ps}
  | "{" fs=lab_list(lab_pat(annot)) "}"                 {`Product ($loc, fs)}
  | "«" t=typ_bid "," p=pat(annot_let) "»" e=annot      {`Pack ($loc, p, t, e)}

//

lab_exp:
  | l=label "=" e=exp                                   {(l, e)}
  | i=exp_rid                                           {(Exp.Var.to_label i, `Var ($loc, i))}

exp_rid:
  | i=Id                                                {Exp.Var.of_string $loc i}

exp_bid:
  | "_"                                                 {Exp.Var.underscore $loc}
  | i=exp_rid                                           {i}

exp_tstr_rest:
  | TstrClose                                           {[]}
  | l=TstrEsc v=exp s=TstrStr es=exp_tstr_rest          {`Exp (Label.of_string $loc(l) l, v) :: `Str s :: es}

exp_tstr:
  | TstrOpenRaw s=TstrStr es=exp_tstr_rest              {`Tstr ($loc, Exp.raw, `Str s :: es)}
  | i=TstrOpen s=TstrStr es=exp_tstr_rest               {`Tstr ($loc, Exp.Var.of_string $loc(i) i, `Str s :: es)}

exp_atom:
  | i=exp_rid                                           {`Var ($loc, i)}
  | l=LitNat                                            {`Const ($loc, `LitNat l)}
  | s=exp_tstr                                          {s}
  | "(" es=list_n(exp, ",") ")"                         {Exp.tuple $loc es}
  | "{" fs=lab_list(lab_exp) "}"                        {`Product ($loc, fs)}
  | f=exp_atom xs=between("_(", list_n(exp, ","), ")")  {`App ($loc, f, xs Exp.tuple)}
  | f=exp_atom "_[" x=typ "]"                           {`Inst ($loc, f, x)}
  | e=exp_atom "." l=label                              {`Select ($loc, e, Exp.atom l)}
  | e=exp_atom "." "(" i=exp ")"                        {`Select ($loc, e, i)}
  | "target" "[" t=typ "]" c=lit_string                 {`Const ($loc, `Target (t, c))}
  | "import" p=lit_string                               {`Import ($loc, p)}

lab_exp_atom:
  | e=exp_atom                                          {e}
  | "'" l=label                                         {Exp.atom l}

exp_app:
  | e=exp_atom                                          {e}
  | f=exp_app x=lab_exp_atom                            {`App ($loc, f, x)}
  | f=exp_app "[" x=typ "]"                             {`Inst ($loc, f, x)}
  | "case" cs=exp_atom                                  {`Case ($loc, cs)}

exp_inf:
  | "'" l=label                                         {Exp.atom l}
  | "'" l=label e=lab_exp_atom                          {`Inject ($loc, l, e)}
  | e=exp_app                                           {e}
  | f=uop x=exp_app                                     {`App ($loc, f, x)}
  | f=exp_inf "◁" x=exp_inf                             {`AppR ($loc, f, x)}
  | x=exp_inf "▷" f=exp_inf                             {`AppL ($loc, x, f)}
  | f=exp_inf "◇" x=exp_inf                             {`App ($loc, f, x)}
  | l=exp_inf "„" r=exp_inf                             {`Merge ($loc, l, r)}
  | l=exp_inf o=bop r=exp_inf                           {`App ($loc, `App ($loc, o, l), r)}

%inline uop:
  | "¬"                                                 {`Const ($loc, `OpLogicalNot)}
  | "+"                                                 {`Const ($loc, `OpArithPlus)}
  | "-"                                                 {`Const ($loc, `OpArithMinus)}

%inline bop:
  | "∨"                                                 {`Const ($loc, `OpLogicalOr)}
  | "∧"                                                 {`Const ($loc, `OpLogicalAnd)}

  | "=" "[" t=typ "]"                                   {`Const ($loc, `OpEq t)}
  | "≠" "[" t=typ "]"                                   {`Const ($loc, `OpEqNot t)}

  | ">"                                                 {`Const ($loc, `OpCmpGt)}
  | "≥"                                                 {`Const ($loc, `OpCmpGtEq)}
  | "<"                                                 {`Const ($loc, `OpCmpLt)}
  | "≤"                                                 {`Const ($loc, `OpCmpLtEq)}

  | "+"                                                 {`Const ($loc, `OpArithAdd)}
  | "-"                                                 {`Const ($loc, `OpArithSub)}
  | "^"                                                 {`Const ($loc, `OpStringCat)}

  | "*"                                                 {`Const ($loc, `OpArithMul)}
  | "/"                                                 {`Const ($loc, `OpArithDiv)}
  | "%"                                                 {`Const ($loc, `OpArithRem)}

exp_bind(head):
  | head p=pat(annot_lam) "." e=exp                     {`LamPat ($loc, p, e)}

exp_in:
  | e=uop "_"                                           {e}
  | e=bop                                               {e}
  | e=exp_inf                                           {e}

exp:
  | e=exp_in                                            {e}
  | e=exp_bind("μ")                                     {`Mu ($loc, e)}
  | e=exp_bind("λ")                                     {e}
  | "Λ" b=typ_bind "." e=exp                            {`Gen ($loc, fst b, snd b, e)}
  | "if" c=exp "then" t=exp "else" e=exp                {`IfElse ($loc, c, t, e)}
  | d=typ_def "in" e=exp                                {`LetDefIn ($loc, d, e)}
  | "let" bs=list_1(par_def, "and") "in" e=exp          {`LetPatPar ($loc, bs, e)}
  | "let" bs=list_1(mu_def, "and") "in" e=exp           {`LetPatRec ($loc, bs, e)}
  | "«" x=typ "," e=exp "»" ":" f=typ                   {`Pack ($loc, x, e, f)}
  | e=exp_in ":" t=typ                                  {`Annot ($loc, e, t)}

mu_def:
  | "μ" p=pat(annot_lam) "=" v=exp                      {(p, v)}

par_def:
  | p=pat(annot_let) "=" v=exp                          {(p, None, v)}
  | p=pat(annot_let) ":" t=typ "=" v=exp                {(p, Some t, v )}

//

mods:
  | e=exp EOF                                           {e}

sigs:
  | t=typ EOF                                           {t}

incs:
  | ds=typ_defs EOF                                     {ds}
