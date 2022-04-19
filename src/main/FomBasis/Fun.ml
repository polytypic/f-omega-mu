include Stdlib.Fun

module Syntax = struct
  let id = id
  let const = const
  let uncurry abc (a, b) = abc a b
  let ( >>> ) ab bc a = bc (ab a)
  let ( <<< ) bc ab a = bc (ab a)
end
