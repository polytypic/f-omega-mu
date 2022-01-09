'use strict'
const $1 = x => x
const $2 = ab => h => ab($1)(h)
const $3 = ab => ab($1)
const $4 = bc => ab => x => bc(ab(x))
const $5 = {
  ofFold: $2,
  ofLeibniz: $1,
  refl: $1,
  symm: $3,
  trans: $4,
  to: $1,
  from: $3,
}
const $6 = bc => ab => f => bc($1)(ab(f))
const $7 = {
  ofFold: $1,
  ofLeibniz: $1,
  refl: $1,
  symm: $1,
  trans: $6,
  to: $3,
  from: $3,
}
;({Leibniz: $5, Fold: $7})
