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
%token BraceLhsNS "_{"
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

list_rev_1(elem, sep):
  | e=elem                                          {[e]}
  | es=list_rev_1(elem, sep) sep e=elem             {e::es}

list_1(elem, sep):
  | es=list_rev_1(elem, sep)                        {List.rev es}

list_n(elem, sep):
  | option(sep)                                     {[]}
  | es=list_rev_1(elem, sep) option(sep)            {List.rev es}

//

kind_atom:
  | "_"                                             {Kind.fresh $loc}
  | "*"                                             {`Star $loc}
  | "(" k=kind ")"                                  {k}

kind:
  | k=kind_atom                                     {k}
  | d=kind_atom "→" c=kind                          {`Arrow ($loc, d, c)}

//

lab:
  | i=Id                                            {Label.of_string $loc i}
  | n=LitNat                                        {Label.of_number $loc n}

//

lit_string:
  | TstrOpenRaw s=TstrStr TstrClose                 {s}

//

path:
  | s=lit_string                                    {($loc, s)}

//

typ_def_mu:
  | "μ" b=typ_bind "=" t=typ                        {(fst b, snd b, t)}

typ_def_par:
  | b=typ_bind "=" t=typ                            {(fst b, snd b, t)}

typ_def:
  | "type" bs=list_1(typ_def_par, "and")            {`TypPar bs}
  | "type" bs=list_1(typ_def_mu, "and")             {`TypRec bs}
  | "include" p=path                                {`Include p}

//

typ_defs:
  | d=typ_def                                       {d :> _ Typ.Defs.f}
  | d=typ_def "in" ds=typ_defs                      {`In ($loc, d, ds)}
  | "local" d=typ_def "in" ds=typ_defs              {`LocalIn ($loc, d, ds)}

//

typ_lab:
  | l=lab ":" t=typ                                 {(l, t)}
  | i=typ_rid                                       {(Typ.Var.to_label i, Typ.var i)}

typ_tick_lab:
  | "'" l=lab                                       {(l, Typ.product $loc [])}
  | "'" l=lab t=typ_atom_tick                       {(l, t)}
  | "'" l=lab t=typ_high_prec                       {(l, t)}

typ_rid:
  | i=Id                                            {Typ.Var.of_string $loc i}
  | i=IdTyp                                         {Typ.Var.of_string $loc i}

typ_bid:
  | "_"                                             {Typ.Var.underscore $loc}
  | i=typ_rid                                       {i}

typ_bind:
  | i=typ_bid                                       {(i, Kind.fresh $loc)}
  | i=typ_bid ":" k=kind                            {(i, k)}

typ_high_prec:
  | "_(" xs=list_n(typ, ",") ")"                    {Typ.tuple $loc xs}
  | "_{" fs=list_n(typ_lab, ",") "}"                {Typ.product $loc fs}

typ_atom:
  | i=typ_rid                                       {Typ.var i}
  | "(" ts=list_n(typ, ",") ")"                     {Typ.tuple $loc ts}
  | "{" fs=list_n(typ_lab, ",") "}"                 {Typ.product $loc fs}
  | f=typ_atom x=typ_high_prec                      {`App ($loc, f, x)}
  | "μ" "(" t=typ ")"                               {`Mu ($loc, t)}
  | "∃" "(" t=typ ")"                               {`Exists ($loc, t)}
  | "∀" "(" t=typ ")"                               {`ForAll ($loc, t)}
  | "import" p=path                                 {`Import p}

typ_tick:
  | "'" l=lab                                       {Typ.atom l}
  | "'" l=lab t=typ_high_prec                       {Typ.sum $loc [(l, t)]}

typ_atom_tick:
  | t=typ_atom                                      {t}
  | t=typ_tick                                      {t}

typ_app:
  | t=typ_atom                                      {t}
  | f=typ_app x=typ_atom_tick                       {`App ($loc, f, x)}

typ_inf:
  | option("|") s=list_1(typ_tick_lab, "|")         {Typ.sum $loc s}
  | "|"                                             {Typ.sum $loc []}
  | t=typ_app                                       {t}
  | l=typ_inf "∨" r=typ_inf                         {`Join ($loc, l, r)}
  | l=typ_inf "∧" r=typ_inf                         {`Meet ($loc, l, r)}

typ_arr:
  | t=typ_inf                                       {t}
  | d=typ_inf "→" c=typ                             {`Arrow ($loc, d, c)}

typ_lam(head):
  | head b=typ_bind "." t=typ                       {`Lam ($loc, fst b, snd b, t)}

typ:
  | t=typ_arr                                       {t}
  | t=typ_lam("μ")                                  {`Mu ($loc, t)}
  | t=typ_lam("∃")                                  {`Exists ($loc, t)}
  | t=typ_lam("∀")                                  {`ForAll ($loc, t)}
  | t=typ_lam("λ")                                  {t}
  | d=typ_def "in" t=typ                            {`Let ($loc, d, t)}

//

ann_let:
  |                                                 {Typ.zero $loc}

ann_lam:
  | ":" t=typ                                       {t}

pat_lab(ann):
  | l=lab "=" p=pat(ann)                            {(l, `Pat p)}
  | l=lab a=ann                                     {(l, `Ann a)}

pat(ann):
  | i=exp_bid a=ann                                 {`Id ($loc, i, a)}
  | "(" ps=list_n(pat(ann), ",") ")"                {Exp.Pat.tuple $loc ps}
  | "{" fs=list_n(pat_lab(ann), ",") "}"            {`Product ($loc, fs)}
  | "«" t=typ_bid "," p=pat(ann_let) "»" e=ann      {`Pack ($loc, p, t, e)}

//

tstr_elem:
  | l=TstrEsc v=exp                                 {`Exp (Label.of_string $loc(l) l, v)}
  | s=TstrStr                                       {`Str s}

tstr_open:
  | TstrOpenRaw                                     {Exp.raw}
  | i=TstrOpen                                      {Exp.Var.of_string $loc i}

tstr:
  | i=tstr_open es=list(tstr_elem) TstrClose        {`Tstr ($loc, i, es)}

//

exp_lab:
  | l=lab "=" e=exp                                 {(l, e)}
  | i=exp_rid                                       {(Exp.Var.to_label i, Exp.var i)}

exp_rid:
  | i=Id                                            {Exp.Var.of_string $loc i}

exp_bid:
  | "_"                                             {Exp.Var.underscore $loc}
  | i=exp_rid                                       {i}

exp_high_prec:
  | "_(" xs=list_n(exp, ",") ")"                    {Exp.tuple $loc xs}
  | "_{" fs=list_n(exp_lab, ",") "}"                {Exp.product $loc fs}

exp_atom:
  | i=exp_rid                                       {Exp.var i}
  | l=LitNat                                        {`Const ($loc, `LitNat l)}
  | e=tstr                                          {e}
  | "(" es=list_n(exp, ",") ")"                     {Exp.tuple $loc es}
  | "{" fs=list_n(exp_lab, ",") "}"                 {Exp.product $loc fs}
  | f=exp_atom x=exp_high_prec                      {`App ($loc, f, x)}
  | f=exp_atom "_[" x=typ "]"                       {`Inst ($loc, f, x)}
  | e=exp_atom "." l=lab                            {`Select ($loc, e, Exp.atom l)}
  | e=exp_atom "." "(" i=exp ")"                    {`Select ($loc, e, i)}
  | "target" "[" t=typ "]" c=lit_string             {`Const ($loc, `Target (t, c))}
  | "import" p=path                                 {`Import p}

exp_tick:
  | "'" l=lab                                       {Exp.atom l}
  | "'" l=lab e=exp_high_prec                       {`Inject ($loc, l, e)}

exp_atom_tick:
  | e=exp_atom                                      {e}
  | e=exp_tick                                      {e}

exp_app:
  | e=exp_atom                                      {e}
  | f=exp_app x=exp_atom_tick                       {`App ($loc, f, x)}
  | f=exp_app "[" x=typ "]"                         {`Inst ($loc, f, x)}
  | "case" cs=exp_atom                              {`Case ($loc, cs)}

exp_inf:
  | e=exp_app                                       {e}
  | e=exp_tick                                      {e}
  | "'" l=lab e=exp_atom_tick                       {`Inject ($loc, l, e)}
  | f=exp_uop x=exp_app                             {`App ($loc, f, x)}
  | f=exp_inf "◁" x=exp_inf                         {`AppR ($loc, f, x)}
  | x=exp_inf "▷" f=exp_inf                         {`AppL ($loc, x, f)}
  | f=exp_inf "◇" x=exp_inf                         {`App ($loc, f, x)}
  | l=exp_inf "„" r=exp_inf                         {`Merge ($loc, l, r)}
  | l=exp_inf o=exp_bop r=exp_inf                   {`App ($loc, `App ($loc, o, l), r)}

%inline exp_uop:
  | "¬"                                             {`Const ($loc, `OpLogicalNot)}
  | "+"                                             {`Const ($loc, `OpArithPlus)}
  | "-"                                             {`Const ($loc, `OpArithMinus)}

%inline exp_bop:
  | "∨"                                             {`Const ($loc, `OpLogicalOr)}
  | "∧"                                             {`Const ($loc, `OpLogicalAnd)}

  | "=" "[" t=typ "]"                               {`Const ($loc, `OpEq t)}
  | "≠" "[" t=typ "]"                               {`Const ($loc, `OpEqNot t)}

  | ">"                                             {`Const ($loc, `OpCmpGt)}
  | "≥"                                             {`Const ($loc, `OpCmpGtEq)}
  | "<"                                             {`Const ($loc, `OpCmpLt)}
  | "≤"                                             {`Const ($loc, `OpCmpLtEq)}

  | "+"                                             {`Const ($loc, `OpArithAdd)}
  | "-"                                             {`Const ($loc, `OpArithSub)}
  | "^"                                             {`Const ($loc, `OpStringCat)}

  | "*"                                             {`Const ($loc, `OpArithMul)}
  | "/"                                             {`Const ($loc, `OpArithDiv)}
  | "%"                                             {`Const ($loc, `OpArithRem)}

exp_bind(head):
  | head p=pat(ann_lam) "." e=exp                   {`LamPat ($loc, p, e)}

exp_in:
  | e=exp_uop "_"                                   {e}
  | e=exp_bop                                       {e}
  | e=exp_inf                                       {e}

exp_def_mu:
  | "μ" p=pat(ann_lam) "=" v=exp                    {(p, v)}

exp_def_par:
  | p=pat(ann_let) "=" v=exp                        {(p, None, v)}
  | p=pat(ann_let) ":" t=typ "=" v=exp              {(p, Some t, v )}

exp_def:
  | d=typ_def                                       {d :> _ Exp.Def.f}
  | "let" bs=list_1(exp_def_par, "and")             {`PatPar bs}
  | "let" bs=list_1(exp_def_mu, "and")              {`PatRec bs}

exp:
  | e=exp_in                                        {e}
  | e=exp_bind("μ")                                 {`Mu ($loc, e)}
  | e=exp_bind("λ")                                 {e}
  | "Λ" b=typ_bind "." e=exp                        {`Gen ($loc, fst b, snd b, e)}
  | "if" c=exp "then" t=exp "else" e=exp            {`IfElse ($loc, c, t, e)}
  | d=exp_def "in" e=exp                            {`Let ($loc, d, e)}
  | "«" x=typ "," e=exp "»" ":" f=typ               {`Pack ($loc, x, e, f)}
  | e=exp_in ":" t=typ                              {`Annot ($loc, e, t)}

//

mods:
  | e=exp EOF                                       {e}

sigs:
  | t=typ EOF                                       {t}

incs:
  | ds=typ_defs EOF                                 {ds}
