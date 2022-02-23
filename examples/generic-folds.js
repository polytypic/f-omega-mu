'use strict'
const ꘌ𖩇string𖩉 = l => r => l === r
const ᐟNil = ['Nil']
const ᐥyᐥ = 'y'
const 𛰝idꘌᐥyᐥ𛰞 = {id: ᐥyᐥ}
const ᐟVar𛰝idꘌᐥyᐥ𛰞 = ['Var', 𛰝idꘌᐥyᐥ𛰞]
const ᐥxᐥ = 'x'
const 𛰝idꘌᐥxᐥ𛰞 = {id: ᐥxᐥ}
const ᐟVar𛰝idꘌᐥxᐥ𛰞 = ['Var', 𛰝idꘌᐥxᐥ𛰞]
const ᐥFᐥ = 'F'
const 𛰝idꘌᐥFᐥ𛰞 = {id: ᐥFᐥ}
const ᐟVar𛰝idꘌᐥFᐥ𛰞 = ['Var', 𛰝idꘌᐥFᐥ𛰞]
const 𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_argꘌᐟVar𛰝idꘌᐥxᐥ𛰞𛰞 = {
  fn: ᐟVar𛰝idꘌᐥFᐥ𛰞,
  arg: ᐟVar𛰝idꘌᐥxᐥ𛰞,
}
const ᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_argꘌᐟVar𛰝idꘌᐥxᐥ𛰞𛰞 = [
  'App',
  𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_argꘌᐟVar𛰝idꘌᐥxᐥ𛰞𛰞,
]
const 𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_ = {
  var: ᐥxᐥ,
  exp: ᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_argꘌᐟVar𛰝idꘌᐥxᐥ𛰞𛰞,
}
const ᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_ = [
  'Abs',
  𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_,
]
const 𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_ = {
  fn: ᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_,
  arg: ᐟVar𛰝idꘌᐥyᐥ𛰞,
}
const ᐟApp𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_ = [
  'App',
  𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_,
]
const _has = p => ᐟNil_ǀ_ᐟCons => {
  const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
  if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
    return false
  } else {
    return p(val_ᐟNil_ǀ_ᐟCons[1]) || _has(p)(val_ᐟNil_ǀ_ᐟCons[2])
  }
}
;(function fold(ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp) {
  const [tag_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp, val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp] =
    ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp
  switch (tag_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp) {
    case 'Var': {
      return ['Cons', {1: val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.id, 2: ᐟNil}]
    }
    case 'Lit': {
      return ᐟNil
    }
    case 'App': {
      const fn = fold(val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.fn)
      const arg = fold(val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.arg)
      const ys = (function fold(ᐟNil_ǀ_ᐟCons) {
        const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
        if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
          return ᐟNil
        } else {
          const $2$ = fold(val_ᐟNil_ǀ_ᐟCons[2])
          if (_has(ꘌ𖩇string𖩉(val_ᐟNil_ǀ_ᐟCons[1]))(fn)) {
            return $2$
          } else {
            return ['Cons', {1: val_ᐟNil_ǀ_ᐟCons[1], 2: $2$}]
          }
        }
      })(arg)
      return (function fold(ᐟNil_ǀ_ᐟCons) {
        const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
        if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
          return ys
        } else {
          const $2$ = fold(val_ᐟNil_ǀ_ᐟCons[2])
          return ['Cons', {1: val_ᐟNil_ǀ_ᐟCons[1], 2: $2$}]
        }
      })(fn)
    }
    default: {
      const exp = fold(val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.exp)
      return (function fold(ᐟNil_ǀ_ᐟCons) {
        const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
        if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
          return ᐟNil
        } else {
          const $2$ = fold(val_ᐟNil_ǀ_ᐟCons[2])
          if (val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.var === val_ᐟNil_ǀ_ᐟCons[1]) {
            return $2$
          } else {
            return ['Cons', {1: val_ᐟNil_ǀ_ᐟCons[1], 2: $2$}]
          }
        }
      })(exp)
    }
  }
})(ᐟApp𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_)
