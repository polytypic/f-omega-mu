'use strict'
const $1 = $1 => $2 => ($1 + $2) | 0
const $2 = c => c.nil
const $3 = c => c.cons(1)($2)
const $4 = c => c.cons(4)($3)
const $5 = c => c.cons(1)($4)
const $6 = c => c.cons(3)($5)
const fold = fn => z => xs =>
  xs({nil: z, cons: x => xs$1 => fold(fn)(fn(x)(z))(xs$1)})
fold($1)(0)($6)
