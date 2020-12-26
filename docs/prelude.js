'use strict'

const iterator = function* () {
  for (let i = 0, n = this.length; i < n; ++i) {
    yield this[i]
  }
}

const writable = Object.freeze({writable: true})
const writableFunction = Object.freeze({
  arguments: writable,
  callee: writable,
  caller: writable,
  length: writable,
  name: writable,
})

const rec = function rec(fn) {
  let knot
  function proxy(x) {
    return knot(x)
  }
  knot = fn(proxy)
  if (typeof knot === 'object') {
    if (Array.isArray(knot)) {
      Object.defineProperty(proxy, 'length', {value: knot.length})
      proxy[Symbol.iterator] = iterator
    } else {
      Object.defineProperties(proxy, writableFunction)
    }
    Object.assign(proxy, knot)
    Object.setPrototypeOf(proxy, Object.getPrototypeOf(knot))
    return proxy
  } else {
    return knot
  }
}
