let curry f x y = f (x, y);;

let average (x, y) = (x +. y) /. 2.0;;

let curried_avg = curry average;;

average (4.0, 5.3);;

curried_avg 4.0 5.3;;

let uncurry f (x, y) = f x y;;

let avg = uncurry curried_avg in avg (4.0, 5.3);;

let rec repeat f n x =
	if n > 0 then repeat f (n-1) (f x) else x;;

let fib n =
	let (fibn, _) =
		repeat (fun (a, b) -> (a + b, a)) n (1, 0)
	in fibn;;


let ($) f g x = f (g x);;

let id x = x;;

(*
	n回関数をカリー化した関数になる
	繰り返し二乗法使ってるからそれはそうだよね
*)
let rec funny f n =
	if n = 0 then id
	else if n mod 2 = 0 then funny (f $ f) (n / 2)
	else funny (f $ f) (n / 2) $ f;;

let twice f x = f (f x);;
(*
	('a -> 'a) -> 'a -> 'a
*)

(*
	twice twice f x
	-> twice


	-> f(f(f(f(x))))
 *)

let f x =
	if x >= 0 then sin else cos;;

let id x = x;;

let k x y = x;;

let s x y z = x z (y z);;

(*
	k (s k k) x y
*)
