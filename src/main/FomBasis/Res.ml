type ('e, 'a) t = [`Ok of 'a | `Error of 'e]

let catch thunk = try `Ok (thunk ()) with e -> `Error e
