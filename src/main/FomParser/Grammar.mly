%token <Bigint.t> LitNat
%token <FomCST.LitString.t> LitString

%token <string> Id
%token <string> IdTyp
%token <string> IdSub
%token <string> Comment

%token And "and"
%token Case "case"
%token Else "else"
%token If "if"
%token Import "import"
%token In "in"
%token Include "include"
%token Let "let"
%token Target "target"
%token Then "then"
%token Type "type"

%token ArrowRight "→"
%token Backslash "\\"
%token BraceLhs "{"
%token BraceRhs "}"
%token BracketLhs "["
%token BracketRhs "]"
%token Colon ":"
%token Comma ","
%token Diamond "◇"
%token Dot "."
%token DoubleAngleLhs "<<"
%token DoubleAngleRhs ">>"
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
%left "+" "-"
%left "*" "/" "%"

%start <Exp.t> program
%start <Typ.t> typ_exp
%start <Typ.t Typ.Def.f list> typ_defs

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

//

kind_atom:
  | "_"                                                 {Kind.fresh $loc}
  | "*"                                                 {`Star $loc}
  | "("k=kind")"                                        {k}

kind:
  | k=kind_atom                                         {k}
  | d=kind_atom"→"c=kind                                {`Arrow ($loc, d, c)}

//

label:
  | i=Id                                                {Label.of_string $loc i}
  | n=LitNat                                            {Label.of_string $loc (Bigint.to_string n)}

lab_list(item):
  | ls=list_n(item,",")                                 {check_lab_list ls}

//

typ_def:
  | "type"b=typ_bind"="t=typ                            {`Typ ($loc, fst b, snd b, t)}
  | "type"bs=list_1(typ_mu_def, "and")                  {`TypRec ($loc, bs)}
  | "include"p=LitString                                {`Include ($loc, p)}

typ_mu_def:
  | "μ"b=typ_bind"="t=typ                               {(fst b, snd b, t)}

//

lab_typ:
  | l=label":"t=typ                                     {(l, t)}
  | i=typ_rid                                           {(Typ.Id.to_label i, `Var ($loc, i))}

tick_lab_typ:
  | "'"l=label                                          {(l, Typ.product $loc [])}
  | "'"l=label t=typ_atom                               {(l, t)}

typ_rid:
  | i=Id                                                {Typ.Id.of_string $loc i}
  | i=IdTyp                                             {Typ.Id.of_string $loc i}

typ_bid:
  | Underscore                                          {Typ.Id.of_string $loc "_"}
  | i=typ_rid                                           {i}

typ_bind:
  | i=typ_bid                                           {(i, Kind.fresh $loc)}
  | i=typ_bid":"k=kind                                  {(i, k)}

typ_atom:
  | i=typ_rid                                           {`Var ($loc, i)}
  | "("ts=list_n(typ,",")")"                            {Typ.tuple $loc ts}
  | "{"fs=lab_list(lab_typ)"}"                          {Typ.product $loc fs}
  | "μ""("t=typ")"                                      {`Mu ($loc, t)}
  | "∃""("t=typ")"                                      {`Exists ($loc, t)}
  | "∀""("t=typ")"                                      {`ForAll ($loc, t)}
  | "import"p=LitString                                 {`Import ($loc, p)}

typ_app:
  | t=typ_atom                                          {t}
  | f=typ_app x=typ_atom                                {`App ($loc, f, x)}

typ_inf:
  | t=typ_app                                           {t}
  | d=typ_app"→"c=typ                                   {`Arrow ($loc, d, c)}

typ_lam(head):
  | head b=typ_bind"."t=typ                             {`Lam ($loc, fst b, snd b, t)}

typ:
  | option("|") s=list_1(tick_lab_typ, "|")             {Typ.sum $loc s}
  | "|"                                                 {Typ.sum $loc []}
  | t=typ_inf                                           {t}
  | t=typ_lam("μ")                                      {`Mu ($loc, t)}
  | t=typ_lam("∃")                                      {`Exists ($loc, t)}
  | t=typ_lam("∀")                                      {`ForAll ($loc, t)}
  | t=typ_lam("λ")                                      {t}
  | d=typ_def"in"t=typ                                  {`LetDefIn ($loc, d, t)}

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
  | "("ps=list_n(pat(annot),",")")"                     {Exp.Pat.tuple $loc ps}
  | "{"fs=lab_list(lab_pat(annot))"}"                   {`Product ($loc, fs)}
  | "<<"t=typ_bid"\\"p=pat(annot_let)">>"e=annot        {`Pack ($loc, p, t, e)}

//

lab_exp:
  | l=label"="e=exp                                     {(l, e)}
  | i=exp_rid                                           {(Exp.Id.to_label i, `Var ($loc, i))}

exp_rid:
  | i=Id                                                {Exp.Id.of_string $loc i}

exp_bid:
  | Underscore                                          {Exp.Id.of_string $loc "_"}
  | i=exp_rid                                           {i}

exp_atom:
  | i=exp_rid                                           {`Var ($loc, i)}
  | l=LitNat                                            {`Const ($loc, `LitNat l)}
  | l=LitString                                         {`Const ($loc, `LitString l)}
  | "("es=list_n(exp,",")")"                            {Exp.tuple $loc es}
  | "{"fs=lab_list(lab_exp)"}"                          {`Product ($loc, fs)}
  | e=exp_atom"."l=label                                {`Select ($loc, e, Exp.atom l)}
  | e=exp_atom".""("i=exp")"                            {`Select ($loc, e, i)}
  | "target""["t=typ"]"c=LitString                      {`Const ($loc, `Target (t, c))}
  | "import"p=LitString                                 {`Import ($loc, p)}

exp_app:
  | e=exp_atom                                          {e}
  | f=exp_app x=exp_atom                                {`App ($loc, f, x)}
  | f=exp_app "'"l=label                                {`App ($loc, f, Exp.atom l)}
  | f=exp_app"["x=typ"]"                                {`Inst ($loc, f, x)}
  | "case"cs=exp_atom                                   {`Case ($loc, cs)}

exp_inf:
  | "'"l=label                                          {Exp.atom l}
  | "'"l=label e=exp_atom                               {`Inject ($loc, l, e)}
  | e=exp_app                                           {e}
  | f=uop x=exp_app                                     {`App ($loc, f, x)}
  | f=exp_inf"◁"x=exp_inf                               {`AppR ($loc, f, x)}
  | x=exp_inf"▷"f=exp_inf                               {`AppL ($loc, x, f)}
  | f=exp_inf"◇"x=exp_inf                               {`App ($loc, f, x)}
  | l=exp_inf o=bop r=exp_inf                           {`App ($loc, `App ($loc, o, l), r)}

%inline uop:
  | "¬"                                                 {`Const ($loc, `OpLogicalNot)}
  | "+"                                                 {`Const ($loc, `OpArithPlus)}
  | "-"                                                 {`Const ($loc, `OpArithMinus)}

%inline bop:
  | "∨"                                                 {`Const ($loc, `OpLogicalOr)}
  | "∧"                                                 {`Const ($loc, `OpLogicalAnd)}

  | "=""["t=typ"]"                                      {`Const ($loc, `OpEq t)}
  | "≠""["t=typ"]"                                      {`Const ($loc, `OpEqNot t)}

  | ">"                                                 {`Const ($loc, `OpCmpGt)}
  | "≥"                                                 {`Const ($loc, `OpCmpGtEq)}
  | "<"                                                 {`Const ($loc, `OpCmpLt)}
  | "≤"                                                 {`Const ($loc, `OpCmpLtEq)}

  | "+"                                                 {`Const ($loc, `OpArithAdd)}
  | "-"                                                 {`Const ($loc, `OpArithSub)}

  | "*"                                                 {`Const ($loc, `OpArithMul)}
  | "/"                                                 {`Const ($loc, `OpArithDiv)}
  | "%"                                                 {`Const ($loc, `OpArithRem)}

exp_bind(head):
  | head p=pat(annot_lam) "." e=exp                     {`LamPat ($loc, p, e)}

exp_in:
  | e=uop"_"                                            {e}
  | e=bop                                               {e}
  | e=exp_inf                                           {e}

exp:
  | e=exp_in                                            {e}
  | e=exp_bind("μ")                                     {`Mu ($loc, e)}
  | e=exp_bind("λ")                                     {e}
  | "Λ"b=typ_bind"."e=exp                               {`Gen ($loc, fst b, snd b, e)}
  | "if"c=exp"then"t=exp"else"e=exp                     {`IfElse ($loc, c, t, e)}
  | d=typ_def"in"e=exp                                  {`LetDefIn ($loc, d, e)}
  | "let"p=pat(annot_let)"="v=exp"in"e=exp              {`LetPat ($loc, p, None, v, e)}
  | "let"p=pat(annot_let)":"t=typ"="v=exp"in"e=exp      {`LetPat ($loc, p, Some t, v, e)}
  | "let"bs=list_1(mu_def, "and")"in"e=exp              {`LetPatRec ($loc, bs, e)}
  | "<<"x=typ"\\"e=exp">>"":"f=typ                      {`Pack ($loc, x, e, f)}
  | e=exp_in":"t=typ                                    {`Annot ($loc, e, t)}

mu_def:
  | "μ"p=pat(annot_lam)"="v=exp                         {(p, v)}

//

program:
  | e=exp EOF                                           {e}

typ_exp:
  | t=typ EOF                                           {t}

typ_defs:
  | ds=separated_list("in", typ_def) EOF                {ds}
