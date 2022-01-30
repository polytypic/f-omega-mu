'use strict'
const $1 = ['None']
const $2 = '42'
const $3 = r => x => {
  r[0] = x
}
const $4 = r => r[0]
const $5 = x => [x]
const $6 = $5(0)
const counter = $6
const $get$ = k$1 => $8 => {
  for (;;) {
    const k = k$1,
      $7 = $8
    const [$9, $10] = $7
    if ($9 === 'Some') {
      const [$11, $12] = k.of($10[1])
      if ($11 === 'Some') {
        return ['Some', $12]
      } else {
        $8 = $10[2]
      }
    } else {
      return $1
    }
  }
}
const id = $4(counter)
$3(counter)((id + 1) | 0)
const id$1 = $4(counter)
$3(counter)((id$1 + 1) | 0)
;({
  1: $get$({
    to: x => ({id, value: x}),
    of: x => {
      if (x.id === id) {
        return ['Some', x.value]
      } else {
        return $1
      }
    },
  })([
    'Some',
    {1: {id: id$1, value: $2}, 2: ['Some', {1: {id, value: 101}, 2: $1}]},
  ]),
  2: $get$({
    to: x => ({id: id$1, value: x}),
    of: x => {
      if (x.id === id$1) {
        return ['Some', x.value]
      } else {
        return $1
      }
    },
  })([
    'Some',
    {1: {id: id$1, value: $2}, 2: ['Some', {1: {id, value: 101}, 2: $1}]},
  ]),
})
