'use strict'

FomSandbox(window)

//

const Cmp = {
  seq: (l, toR) => (l === 0 ? toR() : l),
}

//

const url = `${location.origin}${location.pathname}examples/*`

//

const get = (o, p, ...ps) => (o != null && p != null ? get(o[p], ...ps) : o)

const throttled = (ms, fn) => {
  let timeout = null
  return (...args) => {
    clearTimeout(timeout)
    timeout = setTimeout(fn, ms, ...args)
  }
}

const onWorker = (init, before, onWorker, after) => {
  const code = `(() => {
  (${init})()
  const onWorker = ${onWorker}
  onmessage = ({data}) =>
    onWorker(data, (data, continues = false) => postMessage({data, continues}))
})()`

  let working = false
  let worker

  return () => {
    if (working) {
      worker.terminate()
      worker = undefined
      working = false
    }
    if (worker === undefined) {
      worker = new Worker('evalWorker.js')
      worker.postMessage(code)
      worker.onmessage = ({data: {data, continues}}) => {
        working = continues
        after(data)
      }
    }
    working = true
    worker.postMessage(before())
  }
}

const addMarker = (markers, cm, pos, annot) =>
  markers.push(
    cm.markText(posAsNative(cm, pos.begins), posAsNative(cm, pos.ends), annot)
  )

const clearMarkers = markers => {
  markers.forEach(mark => mark.clear())
  markers.length = 0
}

const getWidth = editor => {
  const charWidth = editor.defaultCharWidth()
  const scrollArea = editor.getScrollInfo()
  const scrollLeft = editor.doc.scrollLeft

  const leftColumn = Math.ceil(scrollLeft > 0 ? scrollLeft / charWidth : 0)
  const rightPosition = scrollLeft + (scrollArea.clientWidth - 30)
  const rightColumn = Math.floor(rightPosition / charWidth)

  return Math.max(0, rightColumn - leftColumn)
}

//

const cmConfig = {
  cursorBlinkRate: 0,
  indentUnit: 2,
  lineNumbers: true,
  mode: 'fom',
  readOnly: true,
  tabSize: 2,
  theme: 'dracula',
}

const jsCM = CodeMirror(jsDiv, {...cmConfig, mode: 'javascript'})

const nextNonSpace = (str, i, dir = 1) => {
  while (0 <= i && i <= str.length) {
    const c = str[i]
    if (c !== ' ' && c !== '\t' && c !== '\n') return c
    i += dir
  }
  return ''
}

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

const getTokenEndingAt = (cm, cursor) => {
  const token = fomCM.getTokenAt(cursor)
  if (!token || token.end < cursor.ch || cursor.ch <= token.start)
    return undefined
  if (token.end !== cursor.ch) {
    token.string = token.string.slice(0, cursor.ch - token.end)
    token.end = cursor.ch
  }
  return token
}

const resultCM = CodeMirror(resultDiv, cmConfig)
const typCM = CodeMirror(typDiv, {
  ...cmConfig,
  lineNumbers: false,
  value: '...',
})
const fomCM = CodeMirror(fomDiv, {
  ...cmConfig,
  extraKeys: {
    Space() {
      let replacement = ' '
      if (replaceSymbolsInput.checked) {
        const newSelections = []
        for (const selection of fomCM.listSelections()) {
          const cursor = selection.head
          const {line, ch} = cursor
          const token = getTokenEndingAt(fomCM, cursor)
          if (!token) {
            replacement = ' '
            break
          }
          const alt = alternatives[token.string]
          if (!alt || (replacement !== ' ' && replacement !== alt)) {
            replacement = ' '
            break
          }
          replacement = alt

          newSelections.push({
            anchor: {line, ch: token.start},
            head: {line, ch: token.end},
          })
        }
        if (replacement !== ' ') fomCM.setSelections(newSelections)
      }
      fomCM.replaceSelection(replacement)
    },
    F2() {
      const cursor = fomCM.getCursor()
      const du = duAt(fomCM, cursor)
      if (du) {
        const selections = []
        let primary = 0
        const at = loc => {
          if (loc.file === url) {
            const begins = posAsNative(fomCM, loc.begins)
            const ends = posAsNative(fomCM, loc.ends)
            if (
              cursor.line === begins.line &&
              begins.ch <= cursor.ch &&
              cursor.ch <= ends.ch
            ) {
              primary = selections.length
            }
            selections.push({anchor: begins, head: ends})
          }
        }
        at(du.def)
        du.uses.forEach(at)
        fomCM.setSelections(selections, primary)
      }
    },
    Tab() {
      if (!maybeComplete()) fomCM.execCommand('defaultTab')
    },
  },
  readOnly: false,
})

const maybeComplete = (options = {}) => {
  if (fomCM.state.completionActive) return false

  const cursor = fomCM.getCursor()
  const token = getTokenEndingAt(fomCM, cursor)

  if (!token) return false

  fomCM.showHint({
    ...options,
    hint(cm, options) {
      const cursor = cm.getCursor()
      const {line} = cursor
      const {string: t, start, end} = getTokenEndingAt(fomCM, cursor)

      const list = [...identifiers, ...Object.keys(alternatives)]
        .filter(id => id !== t && id.indexOf(t) !== -1)
        .sort((l, r) =>
          Cmp.seq(l.indexOf(t) - r.indexOf(t), () => l.localeCompare(r))
        )
      return {list, from: {line, ch: start}, to: {line, ch: end}}
    },
  })
  return true
}

const ignoredKeys = new Set([
  ' ',
  'ArrowDown',
  'ArrowLeft',
  'ArrowRight',
  'ArrowUp',
  'Backspace',
  'Delete',
  'Enter',
  'Escape',
  'Meta',
  'Shift',
  'Tab',
])

fomCM.on('keyup', (_, {key}) => {
  if (ignoredKeys.has(key)) return
  maybeComplete({completeSingle: false})
})

//

const currentDeps = []

let result = {typ: '...', defUses: [], diagnostics: [], dependencies: []}

//

const cmOf = targetFile => {
  if (url === targetFile) return fomCM
  for (const {file, cm} of currentDeps) if (file === targetFile) return cm
  return null
}

const fileOf = targetCM => {
  if (fomCM === targetCM) return url
  for (const {file, cm} of currentDeps) if (cm === targetCM) return file
  return null
}

//

const duMap = {}

const insertDU = (loc, du) => {
  const file = duMap[loc.file] || (duMap[loc.file] = [])
  const begins = loc.begins
  const line = file[begins.line] || (file[begins.line] = [])
  line[begins.ch] = du
}

const unusedMarkers = []
const unusedAnnot = {
  css: 'text-decoration: underline tomato 2px',
  title: 'Unused binding',
}

const prepareDefUses = () => {
  clearMarkers(unusedMarkers)
  for (const file in duMap) {
    delete duMap[file]
  }
  result.defUses.forEach(du => du.uses.forEach(use => insertDU(use, du)))
  result.defUses.forEach(du => {
    insertDU(du.def, du)
    if (
      0 <= du.def.begins.line &&
      !du.annot.startsWith('_') &&
      du.uses.every(
        use =>
          use.file === du.def.file &&
          use.begins.line === du.def.begins.line &&
          use.begins.ch === du.def.begins.ch &&
          use.ends.line === du.def.ends.line &&
          use.ends.ch === du.def.ends.ch
      )
    ) {
      const cm = cmOf(du.def.file)
      if (cm) addMarker(unusedMarkers, cm, du.def, unusedAnnot)
    }
  })
}

const duMarkers = []

const duAt = (cm, cursor, offset) => {
  if (undefined === offset) {
    return duAt(cm, cursor, 0) || duAt(cm, cursor, 1)
  } else {
    const token = cm.getTokenAt({line: cursor.line, ch: cursor.ch + offset})
    const file = fileOf(cm)
    return (
      file &&
      token &&
      token.start <= cursor.ch &&
      cursor.ch <= token.end &&
      get(
        duMap,
        file,
        cursor.line,
        fom.offset32(cm.getLine(cursor.line), token.start)
      )
    )
  }
}

const posAsNative = (cm, {line, ch}) => {
  const input = cm.getLine(line)
  return {line, ch: fom.offset16(input, ch)}
}

const setTyp = (value, {noKeywords} = 0) => {
  if (typCM.getValue() !== value) {
    typDiv.className = noKeywords ? 'no-keywords show' : 'show'
    typCM.setValue(value)
    typDiv.classList.remove('show')
  }
}

const useAnnot = {css: 'background: saddlebrown'}
const defAnnot = {css: 'background: darkgreen'}

const updateDefUses = throttled(100, cm => {
  clearMarkers(duMarkers)
  const du = duAt(cm, cm.getCursor())
  if (du) {
    du.uses.forEach(use => {
      const cm = cmOf(use.file)
      if (cm) addMarker(duMarkers, cm, use, useAnnot)
    })
    const cm = cmOf(du.def.file)
    if (cm) addMarker(duMarkers, cm, du.def, defAnnot)
    setTyp(du.annot)
  } else {
    setTyp(result.typ, {noKeywords: result.diagnostics.length})
  }
})

fomCM.on('cursorActivity', updateDefUses)

//

const updateDeps = () => {
  if (
    result.dependencies.join(',') !== currentDeps.map(dep => dep.file).join(',')
  ) {
    currentDeps.length = 0

    while (depsDl.firstChild) depsDl.removeChild(depsDl.firstChild)

    if (result.dependencies.length === 0) {
      depsDiv.style.display = 'none'
    } else {
      depsDiv.style.display = 'block'

      for (const dep of result.dependencies) {
        const code = document.createElement('code')
        code.innerText = dep
        const dt = document.createElement('dt')
        dt.appendChild(code)
        depsDl.appendChild(dt)

        const dd = document.createElement('dd')
        depsDl.appendChild(dd)
        const depCM = CodeMirror(dd, cmConfig)

        depCM.on('cursorActivity', updateDefUses)

        const xhr = new XMLHttpRequest()
        xhr.onload = () => depCM.setValue(xhr.responseText.trim())
        xhr.open('GET', dep)
        xhr.send()

        currentDeps.push({file: dep, cm: depCM})
      }
    }
  }
}

//

const run = onWorker(
  () => {
    importScripts('FomSandbox.js')
    FomSandbox(self)
    importScripts('prelude.js')
  },
  () => ({js: jsCM.getValue(), width: getWidth(fomCM)}),
  (params, onResult) => {
    try {
      const result = timed('eval', () => eval(params.js))
      onResult(timed('format', () => fom.format(result, params.width)))
    } catch (error) {
      onResult(error.toString())
    }
  },
  result => {
    if (typeof result !== 'string') result = ''
    resultCM.setValue(result)
  }
)

//

const diagnosticMarkers = []

const build = throttled(
  200,
  onWorker(
    () => {
      importScripts('FomSandbox.js')
      FomSandbox(self)
      importScripts('https://unpkg.com/prettier@2.4.1/standalone.js')
      importScripts('https://unpkg.com/prettier@2.4.1/parser-babel.js')
      importScripts('https://unpkg.com/terser@5.9.0/dist/bundle.min.js')
    },
    () => {
      clearMarkers(diagnosticMarkers)
      return {
        url,
        whole: wholeSelect.checked,
        exp: fomCM.getValue(),
        terser: terserInput.checked,
        prettify: prettifyInput.checked,
        width: getWidth(fomCM),
      }
    },
    ({url, whole, exp, terser, prettify, width}, onResult) => {
      let start = timingStart()
      fom.build(
        whole,
        url,
        exp,
        width,
        () => {
          timingEnd('elaborate', start)
          start = timingStart()
        },
        success => {
          timingEnd('def-use', start)
          onResult(success, true)
          start = timingStart()
        },
        failure => {
          timingEnd('defuses', start)
          onResult(failure)
        },
        async js => {
          timingEnd('compile', start)
          try {
            if (terser)
              js = (
                await Terser.minify(js, {
                  compress: {
                    expression: true,
                    keep_fargs: false,
                    pure_getters: true,
                  },
                  mangle: {
                    toplevel: true,
                  },
                  ecma: 2015,
                })
              ).code
            if (prettify)
              js = prettier
                .format(js, {
                  arrowParens: 'avoid',
                  bracketSpacing: false,
                  printWidth: width,
                  parser: 'babel',
                  plugins: [prettierPlugins.babel],
                  singleQuote: true,
                  trailingComma: 'none',
                })
                .trim()
            onResult(js)
          } catch (error) {
            console.error('Prettier failed with error:', error)
            onResult('')
          }
        }
      )
    },
    data => {
      if (typeof data === 'string') {
        jsCM.setValue(data)
        run(data)
      } else {
        result = data

        if (result.diagnostics.length) {
          resultCM.setValue('')
          jsCM.setValue('')

          result.diagnostics.forEach(diagnostic => {
            const cm = cmOf(diagnostic.file)
            if (cm) {
              const css = diagnosticMarkers.length
                ? 'text-shadow: 0px 0px 10px orange'
                : 'text-shadow: 0px 0px 10px red'
              addMarker(diagnosticMarkers, cm, diagnostic, {
                className: 'marker',
                css,
                title: diagnostic.message,
              })
            }
          })
        }

        prepareDefUses()
        updateDefUses(fomCM)

        updateDeps()
      }
    }
  )
)

build()

fomCM.on('change', build)
wholeSelect.onclick = build
terserInput.onclick = build
prettifyInput.onclick = build

let lastWidth = getWidth(fomCM)
window.onresize = () => {
  const width = getWidth(fomCM)
  if (lastWidth !== width) {
    lastWidth = width
    build()
  }
}

//

fomCM.on(
  'change',
  throttled(
    100,
    onWorker(
      () =>
        importScripts(
          'https://cdnjs.cloudflare.com/ajax/libs/lz-string/1.4.4/lz-string.min.js'
        ),
      () => fomCM.getValue(),
      (exp, onResult) => onResult(LZString.compressToEncodedURIComponent(exp)),
      data => history.replaceState(null, '', location.pathname + '#' + data)
    )
  )
)

//

let identifiers = []

fomCM.on(
  'change',
  throttled(
    100,
    onWorker(
      () => {
        importScripts('FomSandbox.js')
        FomSandbox(self)
      },
      () => fomCM.getValue(),
      (exp, onResult) => onResult(fom.identifiers(exp)),
      ids => (identifiers = ids)
    )
  )
)

//

const alternatives = {}

for (const {unicode, ascii, bop} of fom.synonyms())
  alternatives[ascii] = unicode + (bop ? ' ' : '')

for (const [basename, upper, lower, alternate] of [
  ['Alpha', 'A', 'α'],
  ['Beta', 'B', 'β'],
  ['Gamma', 'Γ', 'γ'],
  ['Delta', 'Δ', 'δ'],
  ['Epsilon', 'E', 'ϵ', 'ε'],
  ['Zeta', 'Ζ', 'ζ'],
  ['Eta', 'Η', 'η'],
  ['Theta', 'Θ', 'θ', 'ϑ'],
  ['Iota', 'Ι', 'ι'],
  ['Kappa', 'Κ', 'κ', 'ϰ'],
  ['Lambda', 'Λ', 'λ'],
  ['Mu', 'Μ', 'μ'],
  ['Nu', 'Ν', 'ν'],
  ['Xi', 'Ξ', 'ξ'],
  ['Omicron', 'O', 'ℴ'],
  ['Pi', 'Π', 'π', 'ϖ'],
  ['Rho', 'Ρ', 'ρ', 'ϱ'],
  ['Sigma', 'Σ', 'σ', 'ς'],
  ['Tau', 'Τ', 'τ'],
  ['Upsilon', 'ϒ', 'υ'],
  ['Phi', 'Φ', 'ϕ', 'φ'],
  ['Chi', 'X', 'χ'],
  ['Psi', 'Ψ', 'ψ'],
  ['Omega', 'Ω', 'ω'],
]) {
  alternatives[`\\${basename}`] = upper
  alternatives[`\\${basename.toLowerCase()}`] = lower
  if (alternate) alternatives[`\\var${basename.toLowerCase()}`] = alternate
}

//

examples.forEach(example => {
  const option = document.createElement('option')
  option.value = example
  option.innerText = example.replace(/^.*[/](.*)[.].*$/, '$1').toUpperCase()
  exampleSelect.appendChild(option)
})

const exampleReset = () => {
  fomCM.off('change', exampleReset)
  exampleSelect.value = ''
}

exampleSelect.onchange = () => load(exampleSelect.value)

//

jsSelect.onchange = () => jsCM.refresh()

depsSelect.onchange = () => {
  for (const div of depsDl.querySelectorAll('dd > div'))
    div.CodeMirror.refresh()
}

//

editInput.onchange = () => fomCM.setOption('readOnly', !editInput.checked)

//

const load = path => {
  if (path) {
    const xhr = new XMLHttpRequest()
    xhr.onload = () => {
      if (200 <= xhr.status && xhr.status < 300) {
        fomCM.off('change', exampleReset)
        fomCM.setValue(xhr.responseText.trim())
        fomCM.on('change', exampleReset)
      } else {
        console.warn(`${xhr.statusText} when loading ${path}`)
      }
    }
    xhr.open('GET', path)
    xhr.send()
  }
}

const interpretHash = hash => {
  if (hash.startsWith('*')) {
    load(hash.slice(1))
  } else {
    fomCM.setValue(LZString.decompressFromEncodedURIComponent(hash) || '')
  }
}

interpretHash(location.hash.slice(1))
