module Syntax = struct
  let failwithf fmt = Printf.ksprintf failwith fmt
end
