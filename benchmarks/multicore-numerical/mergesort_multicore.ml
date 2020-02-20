let n = try int_of_string(Sys.argv.(1)) with _ ->  100

let num_domains = try int_of_string(Sys.argv.(2)) with _ ->  4

let d = Atomic.make 0

let rec take n = function
  | [] -> []
  | x::xs -> if (n > 0) then x :: (take (n-1) xs) else []

let rec drop n = function
  | [] -> []
  | x::xs -> if (n = 0) then x::xs
    else if (n > 0) then (drop (n-1) xs)
    else []

let rec merge x y =
  match x, y with
    | [], l -> l
    | l, [] -> l
    | x :: xs , y :: ys ->
      if x < y
          then x :: merge xs (y :: ys)
          else y :: merge (x::xs) ys

let rec msort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let left = take (List.length l/2) l in
    let right = drop (List.length l/2) l in
    if (Atomic.get d < num_domains) then
    begin
    Atomic.incr d;
    (* Printf.printf "%i \n" (Atomic.get d); *)
     let l = msort left in
     let r = Domain.spawn(fun _ -> msort right) in
     let res = Domain.join r in
     merge l res

    end
    else
    begin
    merge (msort left) (msort right)
    end


let rec gen size l =
  if size = 0 then
    List.rev l
  else
    let n = Random.int 1000000 in
    let list = n :: l in
    gen (size - 1) list



let _ =
   let lst = gen n [] in
   msort lst |> ignore;
   (* List.iter (Printf.printf "%i\n") fin; *)
   Gc.print_stat stdout
