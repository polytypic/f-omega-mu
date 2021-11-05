'use strict'

// Error.stackTraceLimit = undefined // Unlimited stack trace on Chrome

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

class ErrorWithContext extends Error {
  constructor(error, context) {
    super(`${error.message} (${context})`)
    this.error = error
  }

  get name() {
    return this.error.name
  }
}

const withContext = (context, thunk) => {
  try {
    return thunk()
  } catch (error) {
    throw new ErrorWithContext(error, context)
  }
}

const tryIn = (thunk, onSuccess, onFailure) => {
  let result
  try {
    result = thunk()
  } catch (error) {
    return onFailure(error)
  }
  return onSuccess(result)
}

onmessage = message => {
  onmessage = null
  eval(message.data)
}
