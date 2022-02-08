'use strict'
const ꘌ𖩇int𖩉 = l => r => l === r
const λcsꓸcsꓸVal_0 = cs => cs.Val(0)
const λcsꓸcsꓸVal_1 = cs => cs.Val(1)
const ᕯ = l => r => (l * r) | 0
const ᜭ = l => r => (l - r) | 0
const λfꓸλcsꓸcsꓸLam_𛰙λxꓸλcsꓸ𛰙𛰙csꓸIf_ = f => cs =>
  cs.Lam(
    x => cs$1 =>
      cs$1.If(cs$2 => cs$2.Bin(ꘌ𖩇int𖩉)(x)(λcsꓸcsꓸVal_0))(λcsꓸcsꓸVal_1)(cs$2 =>
        cs$2.Bin(ᕯ)(x)(cs$3 =>
          cs$3.App(f)(cs$4 => cs$4.Bin(ᜭ)(x)(λcsꓸcsꓸVal_1))
        )
      )
  )
const λcsꓸcsꓸLam_𛰙λfꓸλcsꓸcsꓸLam_ = cs => cs.Lam(λfꓸλcsꓸcsꓸLam_𛰙λxꓸλcsꓸ𛰙𛰙csꓸIf_)
const λcsꓸcsꓸFix_𛰙λcsꓸcsꓸLam_𛰙λfꓸλcsꓸcsꓸLam_ = cs =>
  cs.Fix(λcsꓸcsꓸLam_𛰙λfꓸλcsꓸcsꓸLam_)
const λcsꓸcsꓸVal_5 = cs => cs.Val(5)
const λcsꓸ𛰙csꓸApp_𛰙λcsꓸcsꓸFix_𛰙λcsꓸcsꓸLam_ = cs =>
  cs.App(λcsꓸcsꓸFix_𛰙λcsꓸcsꓸLam_𛰙λfꓸλcsꓸcsꓸLam_)(λcsꓸcsꓸVal_5)
const λxꓸx = x => x
const _eval = e =>
  e({
    Val: λxꓸx,
    Bin: xyz => x => y => xyz(_eval(x))(_eval(y)),
    If: c => t => e$1 => {
      if (_eval(c)) {
        return _eval(t)
      } else {
        return _eval(e$1)
      }
    },
    App: xy => x => _eval(xy)(_eval(x)),
    Lam: f => x => _eval(f(cs => cs.Val(x))),
    Fix: f => {
      const f$1 = _eval(f)
      const g = x => f$1(g)(x)
      return g
    },
  })
_eval(λcsꓸ𛰙csꓸApp_𛰙λcsꓸcsꓸFix_𛰙λcsꓸcsꓸLam_)
