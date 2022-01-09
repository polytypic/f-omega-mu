'use strict'
const $1 = $1 => $2 => $1 === $2
const $2 = cs => cs.Val(0)
const $3 = cs => cs.Val(1)
const $4 = $1 => $2 => ($1 * $2) | 0
const $5 = $1 => $2 => ($1 - $2) | 0
const $6 = f => cs =>
  cs.Lam(
    x => cs$1 =>
      cs$1.If(cs$2 => cs$2.Bin($1)(x)($2))($3)(cs$2 =>
        cs$2.Bin($4)(x)(cs$3 => cs$3.App(f)(cs$4 => cs$4.Bin($5)(x)($3)))
      )
  )
const $7 = cs => cs.Lam($6)
const $8 = cs => cs.Fix($7)
const $9 = cs => cs.Val(5)
const $10 = cs => cs.App($8)($9)
const $11 = x => x
const $eval$ = e =>
  e({
    Val: $11,
    Bin: xyz => x => y => xyz($eval$(x))($eval$(y)),
    If: c => t => e$1 => {
      if ($eval$(c)) {
        return $eval$(t)
      } else {
        return $eval$(e$1)
      }
    },
    App: xy => x => $eval$(xy)($eval$(x)),
    Lam: f => x => $eval$(f(cs => cs.Val(x))),
    Fix: f => {
      const f$1 = $eval$(f)
      const g = x => f$1(g)(x)
      return g
    },
  })
$eval$($10)
