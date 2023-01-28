export const getTokenEndingAt = (cm, cursor) => {
  const token = cm.getTokenAt(cursor)
  if (!token || token.end < cursor.ch || cursor.ch <= token.start)
    return undefined
  if (token.end !== cursor.ch) {
    token.string = token.string.slice(0, cursor.ch - token.end)
    token.end = cursor.ch
  }
  return token
}

export const getWidth = cm => {
  const charWidth = cm.defaultCharWidth()
  const scrollArea = cm.getScrollInfo()
  const scrollLeft = cm.doc.scrollLeft

  const leftColumn = Math.ceil(scrollLeft > 0 ? scrollLeft / charWidth : 0)
  const rightPosition = scrollLeft + (scrollArea.clientWidth - 30)
  const rightColumn = Math.floor(rightPosition / charWidth)

  return Math.max(0, rightColumn - leftColumn)
}
