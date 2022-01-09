'use strict'
const $1 = x => x(x)
const $2 = x => x
const $3 = k => k($2)
const $4 = k => k($3)
;({unquote: $1, size: 5, cps: $4, nf: true})
