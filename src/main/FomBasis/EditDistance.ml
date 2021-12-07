open Compare.Syntax

type t = int array

let distances ~pat ~txt ~pat_uc ~txt_uc =
  let pat_n = Array.length pat and txt_n = Array.length txt in
  let cost = Array.init pat_n (fun i -> (i + 1) * 2) in
  let ds = Array.make (txt_n + 3) (2 * pat_n) in
  for txt_i = 0 to txt_n - 1 do
    let rec loop pC nC pat_i =
      if pat_i < pat_n then (
        let nC =
          if pat_uc.(pat_i) = txt_uc.(txt_i) then
            if pat.(pat_i) = txt.(txt_i) then pC else pC + 1
          else
            let nC = if pC < nC then pC else nC in
            let nC = if cost.(pat_i) < nC then cost.(pat_i) else nC in
            nC + 2
        in
        let pC = cost.(pat_i) in
        cost.(pat_i) <- nC;
        loop pC nC (pat_i + 1))
      else (
        ds.(txt_i + 3) <- nC;
        if nC < ds.(0) then (
          ds.(0) <- nC;
          ds.(1) <- txt_i))
    in
    loop 0 0 0
  done;
  ds

let compare ls rs =
  Int.compare ls.(0) rs.(0) <>? fun () ->
  Int.compare ls.(1) rs.(1) <>? fun () ->
  let ls_n = Array.length ls and rs_n = Array.length rs in
  let n = Int.max ls_n rs_n in
  let rec lp i =
    if i < n then
      let sub xs xs_n =
        if i < xs_n then xs.(i)
        else Int.min (xs.(xs_n - 1) + (2 * (i - xs_n + 1))) xs.(2)
      in
      let l = sub ls ls_n and r = sub rs rs_n in
      if l < r then -1 else if r < l then 1 else lp (i + 1)
    else 0
  in
  lp 3

let are_unrelated ds = Array.length ds < 3 || ds.(2) = ds.(0)
