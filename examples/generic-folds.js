'use strict'
const ᐟNone = ['None']
const ꘌ𖩇string𖩉 = l => r => l === r
const ᐥxᐥ = 'x'
const ᐥFᐥ = 'F'
const 𛰝idꘌᐥFᐥ𛰞 = {id: ᐥFᐥ}
const ᐟVar𛰝idꘌᐥFᐥ𛰞 = ['Var', 𛰝idꘌᐥFᐥ𛰞]
const 𛰝idꘌᐥxᐥ𛰞 = {id: ᐥxᐥ}
const ᐟVar𛰝idꘌᐥxᐥ𛰞 = ['Var', 𛰝idꘌᐥxᐥ𛰞]
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
const ᐥyᐥ = 'y'
const 𛰝idꘌᐥyᐥ𛰞 = {id: ᐥyᐥ}
const ᐟVar𛰝idꘌᐥyᐥ𛰞 = ['Var', 𛰝idꘌᐥyᐥ𛰞]
const 𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_ = {
  fn: ᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_,
  arg: ᐟVar𛰝idꘌᐥyᐥ𛰞,
}
const ᐟApp𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_ = [
  'App',
  𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_,
]
const has = p => ᐟNone_ǀ_ᐟSome => {
  const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
  if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
    return p(val_ᐟNone_ǀ_ᐟSome[1]) || has(p)(val_ᐟNone_ǀ_ᐟSome[2])
  } else {
    return false
  }
}
;(function fold(ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp) {
  const [tag_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp, val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp] =
    ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp
  switch (tag_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp) {
    case 'Var': {
      return ['Some', {1: val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.id, 2: ᐟNone}]
    }
    case 'Lit': {
      return ᐟNone
    }
    case 'App': {
      const fn = fold(val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.fn)
      const arg = fold(val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.arg)
      const ys = (function fold(ᐟNone_ǀ_ᐟSome) {
        const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
        if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
          const $2$ = fold(val_ᐟNone_ǀ_ᐟSome[2])
          if (has(ꘌ𖩇string𖩉(val_ᐟNone_ǀ_ᐟSome[1]))(fn)) {
            return $2$
          } else {
            return ['Some', {1: val_ᐟNone_ǀ_ᐟSome[1], 2: $2$}]
          }
        } else {
          return ᐟNone
        }
      })(arg)
      return (function fold(ᐟNone_ǀ_ᐟSome) {
        const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
        if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
          const $2$ = fold(val_ᐟNone_ǀ_ᐟSome[2])
          return ['Some', {1: val_ᐟNone_ǀ_ᐟSome[1], 2: $2$}]
        } else {
          return ys
        }
      })(fn)
    }
    default: {
      const exp = fold(val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.exp)
      return (function fold(ᐟNone_ǀ_ᐟSome) {
        const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
        if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
          const $2$ = fold(val_ᐟNone_ǀ_ᐟSome[2])
          if (val_ᐟLit_ǀ_ᐟVar_ǀ_ᐟAbs_ǀ_ᐟApp.var === val_ᐟNone_ǀ_ᐟSome[1]) {
            return $2$
          } else {
            return ['Some', {1: val_ᐟNone_ǀ_ᐟSome[1], 2: $2$}]
          }
        } else {
          return ᐟNone
        }
      })(exp)
    }
  }
})(ᐟApp𛰝fnꘌᐟAbs𛰝varꘌᐥxᐥꓹ_expꘌᐟApp𛰝fnꘌᐟVar𛰝idꘌᐥFᐥ𛰞ꓹ_)
