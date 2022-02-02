'use strict'
const λ_ꓸfalse = _ => false
const λsꓸs = s => s
const Insert = _𛰙sꓹ_n𛰚 => {
  if (_𛰙sꓹ_n𛰚[1].contains(_𛰙sꓹ_n𛰚[2])) {
    return _𛰙sꓹ_n𛰚[1]
  } else {
    const $this$ = {
      isEmpty: false,
      contains: i => i === _𛰙sꓹ_n𛰚[2] || _𛰙sꓹ_n𛰚[1].contains(i),
      insert: i => Insert({1: $this$, 2: i}),
      union: s => Union({1: $this$, 2: s}),
    }
    return $this$
  }
}
const Union = _𛰙s1ꓹ_s2𛰚 => {
  const $this$ = {
    isEmpty: _𛰙s1ꓹ_s2𛰚[1].isEmpty && _𛰙s1ꓹ_s2𛰚[2].isEmpty,
    contains: i => _𛰙s1ꓹ_s2𛰚[1].contains(i) || _𛰙s1ꓹ_s2𛰚[2].contains(i),
    insert: i => Insert({1: $this$, 2: i}),
    union: s => Union({1: $this$, 2: s}),
  }
  return $this$
}
const Empty = {
  isEmpty: true,
  contains: λ_ꓸfalse,
  insert: i => Insert({1: Empty, 2: i}),
  union: λsꓸs,
}
Empty.insert(4).union(Empty.insert(2))
