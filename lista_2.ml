let rec fold_right f xs acc = match xs with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc);;

let rec fold_left f acc xs = match xs with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t;;

Zadanie 1

let length xs = List.fold_left (fun x -> x + 1) 0 xs;;

let rev = List.fold_left (fun x acc -> acc::x) [];;

let map f xs = List.fold_right (fun x acc -> (f x)::acc) xs [];;

let append xs1 xs2 = List.fold_right (fun x acc -> x::acc) xs1 xs2;;

let rev_append xs1 xs2 = List.fold_left (fun acc x -> x::acc) xs2 xs1;;

let rev_map f xs = List.fold_left (fun acc x -> (f x)::acc) [] xs1;;

let filter f l = List.fold_right (fun x acc -> if f x then x::acc else acc) l [];;

Zadanie 2

let rec sublists xs =
  match xs with
  | [] -> [[]]
  | hd::tl -> let z = (sublists tl) in
    List.fold_left (fun acc x -> (hd :: x) :: acc) z z;;

let rec fold_left f xs acc = match xs with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t;;

let list  = [1; 2; 3];;

fold [2; 3] [2; 3]        [[2]; [2; 3]; [3]; []]

Zadanie 3

let suffixes l =
  List.fold_right (fun x acc -> (x :: (List.hd acc)) :: acc)
  l [[]];;

let preffixes l = List.rev 
                    (List.map List.rev 
                        (suffixes (List.rev l)));;

[1;2;3]
(f 1 (f 2 (f 3 [[]]))) =
(f 1 (f 2 ((3::[]) :: [[]]))) =
(f 1 (f 2 ([3] :: [[]]))) =
(f 1 (f 2 [[3], []])) =
(f 1 ((2::[3]) :: [[3], []])) =
(f 1 ([2,3] :: [[3], []])) =
(f 1 [[2,3], [3], []]) =
((1::[2,3]) :: [[2,3], [3], []]) =
([1,2,3] :: [[2,3], [3], []]) =
[[1,2,3], [2,3], [3], []]

Zadanie 4

let rec merge cmp x y = match x, y with 
  | [],_ -> y
  | _,[] -> x
  | xh::xt, yh::yt -> 
    if cmp xh yh
    then xh::(merge cmp xt y)
    else yh::(merge cmp x yt);;

let rec split x y z = match x with
  | [] -> (y,z)
  | x::resto -> split resto z (x::y);;

let rec mergesort cmp x = match x with
  | ([] | _::[]) -> x
  | _ -> let (first,second) = split x [] [] 
     in (merge cmp (mergesort cmp first) 
        (mergesort cmp second));;