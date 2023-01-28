export const alternatives = Object.create(null)

for (const {unicode, ascii, bop} of fom.synonyms())
  alternatives[ascii] = unicode + (bop ? ' ' : '')

for (const [basename, upper, lower, alternate] of [
  ['Alpha', 'A', 'α'],
  ['Beta', 'B', 'β'],
  ['Gamma', 'Γ', 'γ'],
  ['Delta', 'Δ', 'δ'],
  ['Epsilon', 'E', 'ϵ', 'ε'],
  ['Zeta', 'Ζ', 'ζ'],
  ['Eta', 'Η', 'η'],
  ['Theta', 'Θ', 'θ', 'ϑ'],
  ['Iota', 'Ι', 'ι'],
  ['Kappa', 'Κ', 'κ', 'ϰ'],
  ['Lambda', 'Λ', 'λ'],
  ['Mu', 'Μ', 'μ'],
  ['Nu', 'Ν', 'ν'],
  ['Xi', 'Ξ', 'ξ'],
  ['Omicron', 'O', 'ℴ'],
  ['Pi', 'Π', 'π', 'ϖ'],
  ['Rho', 'Ρ', 'ρ', 'ϱ'],
  ['Sigma', 'Σ', 'σ', 'ς'],
  ['Tau', 'Τ', 'τ'],
  ['Upsilon', 'ϒ', 'υ'],
  ['Phi', 'Φ', 'ϕ', 'φ'],
  ['Chi', 'X', 'χ'],
  ['Psi', 'Ψ', 'ψ'],
  ['Omega', 'Ω', 'ω'],
]) {
  alternatives[`\\${basename}`] = upper
  alternatives[`\\${basename.toLowerCase()}`] = lower
  if (alternate) alternatives[`\\var${basename.toLowerCase()}`] = alternate
}
