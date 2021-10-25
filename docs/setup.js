'use strict'

FomSandbox(window)

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
  onmessage = ({data}) => onWorker(data, postMessage)
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
      worker.onmessage = message => {
        working = false
        after(message.data)
      }
    }
    working = true
    worker.postMessage(before())
  }
}

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
  readOnly: 'nocursor',
  tabSize: 2,
  theme: 'dracula',
}

const jsCM = CodeMirror(jsDiv, {...cmConfig, mode: 'javascript'})

CodeMirror.defineMode('fom', () => ({
  startState: () => ({state: fom.initial}),
  token: (stream, state) => {
    const input = stream.string.slice(stream.start)
    const token = fom.token(input, state.state)
    state.state = token.state
    if (token.name === 'error') {
      stream.skipToEnd()
    } else {
      stream.start += fom.offset16(input, token.begins)
      stream.pos += fom.offset16(input, token.ends)
    }
    if (token.name === 'variable' && stream.string[stream.start - 1] === "'")
      return 'property'
    return token.name
  },
}))

const resultCM = CodeMirror(resultDiv, cmConfig)
const typCM = CodeMirror(typDiv, {
  ...cmConfig,
  lineNumbers: false,
  value: '...',
})
const fomCM = CodeMirror(fomDiv, {...cmConfig, readOnly: false})

fomCM.setOption('extraKeys', {
  Tab: () => {
    const spaces = Array(fomCM.getOption('indentUnit') + 1).join(' ')
    fomCM.replaceSelection(spaces)
  },
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

const prepareDefUses = () => {
  for (const file in duMap) {
    delete duMap[file]
  }
  result.defUses.forEach(du => du.uses.forEach(use => insertDU(use, du)))
  result.defUses.forEach(du => insertDU(du.def, du))
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

const updateDefUses = throttled(100, cm => {
  clearMarkers(duMarkers)
  const du = duAt(cm, cm.getCursor())
  if (du) {
    du.uses.forEach(use => {
      const cm = cmOf(use.file)
      if (cm) {
        duMarkers.push(
          cm.markText(posAsNative(cm, use.begins), posAsNative(cm, use.ends), {
            css: 'background: blue',
          })
        )
      }
    })
    const cm = cmOf(du.def.file)
    if (cm) {
      duMarkers.push(
        cm.markText(
          posAsNative(cm, du.def.begins),
          posAsNative(cm, du.def.ends),
          {css: 'background: darkgreen'}
        )
      )
    }
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

const compile = onWorker(
  () => {
    importScripts('FomSandbox.js')
    FomSandbox(self)
    importScripts('https://unpkg.com/prettier@2.3.0/standalone.js')
    importScripts('https://unpkg.com/prettier@2.3.0/parser-babel.js')
  },
  () => ({
    url,
    whole: wholeSelect.checked,
    exp: fomCM.getValue(),
    width: getWidth(fomCM),
  }),
  (params, onResult) => {
    const start = timingStart()
    fom.compile(params.whole, params.url, params.exp, js => {
      timingEnd('compile', start)
      try {
        onResult(
          prettier
            .format(js, {
              arrowParens: 'avoid',
              bracketSpacing: false,
              printWidth: params.width,
              parser: 'babel',
              plugins: [prettierPlugins.babel],
              singleQuote: true,
              trailingComma: 'none',
            })
            .trim()
        )
      } catch (error) {
        console.error('Prettier failed with error:', error)
      }
    })
  },
  js => {
    jsCM.setValue(js)
    run(js)
  }
)

//

const diagnosticMarkers = []

const check = throttled(
  200,
  onWorker(
    () => {
      importScripts('FomSandbox.js')
      FomSandbox(self)
    },
    () => {
      clearMarkers(diagnosticMarkers)
      const width = Math.min(80, (getWidth(fomCM) * 0.85) | 0)
      return {url, exp: fomCM.getValue(), width: width}
    },
    (params, onResult) => {
      const start = timingStart()
      fom.check(params.url, params.exp, params.width, result => {
        timingEnd('check', start)
        onResult(result)
      })
    },
    data => {
      result = data

      if (result.diagnostics.length) {
        resultCM.setValue('')
        jsCM.setValue('')

        result.diagnostics.forEach(diagnostic => {
          const cm = cmOf(diagnostic.file)
          if (cm) {
            const css = cm.getAllMarks().length
              ? 'text-shadow: 0px 0px 10px orange'
              : 'text-shadow: 0px 0px 10px red'
            diagnosticMarkers.push(
              cm.markText(
                posAsNative(cm, diagnostic.begins),
                posAsNative(cm, diagnostic.ends),
                {className: 'marker', css, title: diagnostic.message}
              )
            )
          }
        })
      } else {
        compile()
      }

      prepareDefUses()
      updateDefUses(fomCM)

      updateDeps()
    }
  )
)

check()

fomCM.on('change', check)
wholeSelect.onclick = check

let lastWidth = getWidth(fomCM)
window.onresize = () => {
  const width = getWidth(fomCM)
  if (lastWidth !== width) {
    lastWidth = width
    check()
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

fomCM.on('keyup', (_, event) => {
  if (event.key === ' ' && replaceSymbolsInput.checked) {
    for (const selection of fomCM.listSelections().reverse()) {
      const cursor = selection.head
      const line = cursor.line
      const ch = cursor.ch - 1
      const token = fomCM.getTokenAt({line: line, ch: ch})
      if (!token || token.end !== ch) return
      const replacement = alternatives[token.string]
      if (replacement) {
        fomCM.setSelection({line, ch: token.start}, {line, ch: ch + 1})
        fomCM.replaceSelection(replacement)
      }
    }
  } else if (event.key === 'F2') {
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
  }
})

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

editInput.onchange = () =>
  fomCM.setOption('readOnly', editInput.checked ? false : 'nocursor')

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
