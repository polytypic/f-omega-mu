'use strict'

const timingStart = () => performance.now()
const timingEnd = (message, start) => {
  console.log(message + ' took ' + ((performance.now() - start) | 0) + 'ms')
}

const timed = (message, thunk) => {
  const start = timingStart()
  const result = thunk()
  timingEnd(message, start)
  return result
}

onmessage = message => {
  onmessage = null
  eval(message.data)
}
