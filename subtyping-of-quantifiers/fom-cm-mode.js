import {nextNonSpace} from './util.js'

CodeMirror.defineMode('fom', () => ({
  startState: () => ({previous: '', state: fom.initial}),
  token: (stream, state) => {
    const start = stream.start

    const input = stream.string.slice(stream.start)
    const token = fom.token(input, state.state)

    state.state = token.state

    if (token.name === 'error') {
      stream.skipToEnd()
    } else {
      stream.start += fom.offset16(input, token.begins)
      stream.pos += fom.offset16(input, token.ends)
    }

    if (
      token.name === 'variable' &&
      (nextNonSpace(stream.string, stream.start - 1, -1) === "'" ||
        ((nextNonSpace(stream.string, stream.pos) === ':' ||
          nextNonSpace(stream.string, stream.pos) === '=') &&
          (state.previous === 'punctuation' ||
            (stream.start &&
              !nextNonSpace(stream.string, stream.start - 1, -1)))))
    ) {
      token.name = 'property'
    }

    state.previous = token.name

    return token.name
  },
}))
