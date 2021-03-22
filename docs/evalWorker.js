'use strict'

const timed = (message, thunk) => {
  const start = performance.now()
  const result = thunk()
  console.log(message + ' took ' + ((performance.now() - start) | 0) + 'ms')
  return result
}

onmessage = function (message) {
  onmessage = null
  eval(message.data)
}
