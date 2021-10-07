include Stdlib.Fun

module Syntax = struct
  let id = id
  let const = const
  let ( >>> ) ab bc a = bc (ab a)
  let ( <<< ) bc ab a = bc (ab a)
end
