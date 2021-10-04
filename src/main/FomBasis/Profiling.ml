module Counter = struct
  type t = {mutable count : int; name : string}

  let counters = ref []

  let register name =
    let counter = {name; count = 0} in
    counters := counter :: !counters;
    counter

  let inc counter = counter.count <- counter.count + 1
  let reset_all () = !counters |> List.iter @@ fun counter -> counter.count <- 0

  let dump_all () =
    match !counters with
    | [] -> Printf.printf "No profiling counters.\n"
    | cs ->
      Printf.printf "Profiling counters:\n";
      cs
      |> List.sort (fun l r -> Int.compare r.count l.count)
      |> List.iter @@ fun counter ->
         Printf.printf "  %s: %d\n" counter.name counter.count

  let wrap'1 name fn =
    let counter = register name in
    fun x ->
      inc counter;
      fn x

  let wrap'2 name fn =
    let counter = register name in
    fun x y ->
      inc counter;
      fn x y

  let wrap'3 name fn =
    let counter = register name in
    fun x y z ->
      inc counter;
      fn x y z

  let wrap'4 name fn =
    let counter = register name in
    fun x y z w ->
      inc counter;
      fn x y z w
end
