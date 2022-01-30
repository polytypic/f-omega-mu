'use strict'
const $1 = ['None']
const $2 = {1: 3, 2: $1}
const $3 = ['Some', $2]
const $4 = {1: 1, 2: $3}
const $5 = ['Some', $4]
const $6 = {1: 5, 2: $5}
const $7 = ['Some', $6]
const to_list = $9 => {
  const [$10, $11] = $9
  if ($10 === 'Some') {
    return ['Some', {1: $11[1], 2: to_list($11[2])}]
  } else {
    return $1
  }
}
to_list($7)
