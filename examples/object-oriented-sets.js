'use strict'
const $1 = _ => false
const $2 = s => s
const Insert = $4 => {
  if ($4[1].contains($4[2])) {
    return $4[1]
  } else {
    const $this$ = {
      isEmpty: false,
      contains: i => i === $4[2] || $4[1].contains(i),
      insert: i => Insert({1: $this$, 2: i}),
      union: s => Union({1: $this$, 2: s}),
    }
    return $this$
  }
}
const Union = $4 => {
  const $this$ = {
    isEmpty: $4[1].isEmpty && $4[2].isEmpty,
    contains: i => $4[1].contains(i) || $4[2].contains(i),
    insert: i => Insert({1: $this$, 2: i}),
    union: s => Union({1: $this$, 2: s}),
  }
  return $this$
}
const Empty = {
  isEmpty: true,
  contains: $1,
  insert: i => Insert({1: Empty, 2: i}),
  union: $2,
}
Empty.insert(4).union(Empty.insert(2))
