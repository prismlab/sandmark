let rec gen l n =
  if (n = 0) then List.rev l
  else gen (n :: l) (n-1)

let n = try int_of_string Sys.argv.(1) with _ -> 100

let prime n =
    let rec checkZero x d = match d with
        | 1 -> true
        | _ -> (x mod d <> 0) && checkZero x (d-1)
    in match n with
    | 0 | 1 -> false
    | _ -> checkZero n (n-1)

let rec take n = function
| [] -> []
| x :: xs ->  if n = 0 then [] else x :: (take (n-1) xs)

let rec drop n = function
| [] -> []
| x :: xs -> if n = 0 then (x :: xs) else (drop (n-1) xs)

let slice lst start range = take range (drop start lst)

let number_domains = try int_of_string Sys.argv.(2) with _ -> 4

let rec worker n l st inc =
  if (n = 0) then [] else
  Domain.spawn(fun _ -> List.filter (prime) (slice l st inc ) )
  :: (worker (n - 1) l (st+inc) inc)

let _ =
  let lst = gen [] n in
  let domains = worker number_domains lst 0 (n / number_domains) in
  let res = List.map Domain.join domains in
  let final = List.fold_left (@) [] res in
  List.iter (Printf.printf "%d\n") final;
  Gc.print_stat stdout
