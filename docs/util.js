export const cmps = (l, toR) => (l === 0 ? toR() : l)

export const get = (o, p, ...ps) =>
  o != null && p != null ? get(o[p], ...ps) : o

export const nextNonSpace = (str, i, dir = 1) => {
  while (0 <= i && i < str.length) {
    const c = str[i]
    if (c !== ' ' && c !== '\t' && c !== '\n') return c
    i += dir
  }
  return ''
}

export const throttled = (ms, fn) => {
  let timeout = null
  return (...args) => {
    clearTimeout(timeout)
    timeout = setTimeout(fn, ms, ...args)
  }
}
