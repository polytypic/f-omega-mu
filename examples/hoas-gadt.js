'use strict'
const ꘌ𖩇int𖩉 = l => r => l === r
const 𝛌csꓸcsꓸVal_0 = cs => cs.Val(0)
const 𝛌csꓸcsꓸVal_1 = cs => cs.Val(1)
const ᕯ = l => r => (l * r) | 0
const ᜭ = l => r => (l - r) | 0
const 𝛌fꓸλcsꓸcsꓸLam_𛰙λxꓸλcsꓸ𛰙𛰙csꓸIf_ = f => cs =>
  cs.Lam(
    x => cs$1 =>
      cs$1.If(cs$2 => cs$2.Bin(ꘌ𖩇int𖩉)(x)(𝛌csꓸcsꓸVal_0))(𝛌csꓸcsꓸVal_1)(cs$2 =>
        cs$2.Bin(ᕯ)(x)(cs$3 =>
          cs$3.App(f)(cs$4 => cs$4.Bin(ᜭ)(x)(𝛌csꓸcsꓸVal_1))
        )
      )
  )
const 𝛌csꓸcsꓸLam_𛰙𝛌fꓸλcsꓸcsꓸLam_ = cs => cs.Lam(𝛌fꓸλcsꓸcsꓸLam_𛰙λxꓸλcsꓸ𛰙𛰙csꓸIf_)
const 𝛌csꓸcsꓸFix_𛰙𝛌csꓸcsꓸLam_𛰙𝛌fꓸλcsꓸcsꓸLam_ = cs =>
  cs.Fix(𝛌csꓸcsꓸLam_𛰙𝛌fꓸλcsꓸcsꓸLam_)
const 𝛌csꓸcsꓸVal_5 = cs => cs.Val(5)
const 𝛌csꓸ𛰙csꓸApp_𛰙𝛌csꓸcsꓸFix_𛰙𝛌csꓸcsꓸLam_ = cs =>
  cs.App(𝛌csꓸcsꓸFix_𛰙𝛌csꓸcsꓸLam_𛰙𝛌fꓸλcsꓸcsꓸLam_)(𝛌csꓸcsꓸVal_5)
const 𝛌xꓸx = x => x
const _eval = e =>
  e({
    Val: 𝛌xꓸx,
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
_eval(𝛌csꓸ𛰙csꓸApp_𛰙𝛌csꓸcsꓸFix_𛰙𝛌csꓸcsꓸLam_)
