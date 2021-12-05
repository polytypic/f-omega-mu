export const addMarker = (markers, cm, pos, annot) =>
  markers.push(
    cm.markText(posAsNative(cm, pos.begins), posAsNative(cm, pos.ends), annot)
  )

export const clearMarkers = markers => {
  markers.forEach(mark => mark.clear())
  markers.length = 0
}

export const posAsNative = (cm, {line, ch}) => {
  const input = cm.getLine(line)
  return {line, ch: fom.offset16(input, ch)}
}
