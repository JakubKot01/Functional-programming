Zadanie 1
Funkcja fun x -> x ma typ 'a -> 'a
Funkcja identycznościowa int -> int : fun x -> x + 0

(’a -> ’b) -> (’c -> ’a) -> ’c -> ’b:
	let compose f g x = f(g(x)) ;;
	
'a -> 'b -> 'a:
	let f a b = a ;;
	
'a -> 'a -> 'a:
	let f a b = if a = b
			then a
			else b ;;
	
	
Zadanie 2
	let rec f x = f x ;;
	
Zadanie 3

	let rec s n = n;;

	let hd s = s 0 ;;
	
	let tl s = fun n -> s (n + 1) ;;
	
	let add s x = fun n -> (s n) + x ;;

	let map f s = fun n -> f (s n) ;;
	
	let map2 f s1 s2 = fun n -> f (s1 n) (s2 n) ;;
	
	let replace n a s = fun k -> if k = n
					then a
					else s k ;;
	let take_every n s = fun k -> s (n * k);;
	
Zadanie 4
	let rec scan f a s = fun k -> if k = 0
					then (f a (s 0))
					else (f ((scan f a s) (k - 1)) (s k));;
					
Zadanie 5
	let rec tabulate ?(start=0) stop s = if start > stop
					then []
					else s start :: (tabulate ~start:(start+1) stop s);;

Zadanie 6
	let ctrue (tt: 'a) (ff: 'a) = tt ;;
	let cfalse (tt: 'a) (ff: 'a) = ff ;;
	let cand (p: 'a -> 'a -> 'a) (q: 'a -> 'a -> 'a) (tt: 'a) (ff: 'a) = p (q tt ff) ff ;;
	let cand p1 p2 = cif p1 p2 false ;;
	let cor (p: 'a -> 'a -> 'a) (q: 'a -> 'a -> 'a) (tt: 'a) (ff: 'a) = p tt (q tt ff) ;;
	let cor p1 p2 = cif p1 true p2 ;;

	let cbool_of_bool =
		function
		| true  -> ctrue
		| false -> cfalse;;
	let cbool_of_bool p tt ff = if p then tt else ff ;;
	
	let bool_of_cbool (p: 'bool -> 'bool -> 'bool) = p true false ;;

	let cif p1 tt ff = p1 tt ff ;;

Zadanie 7
	let zero (f: 'a -> 'a) (x: 'a) = x ;;