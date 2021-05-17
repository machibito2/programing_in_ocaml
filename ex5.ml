(*
	t
	f
		型が違うからダメ
	t
	f
		右結合だからダメ
	t
	t
		fun x -> xの型が最初は不明だけど2つ目の関数でbool->boolに束縛される
*)

let rec downto1 = function
	| 0 -> []
	| x -> x :: downto1 (x - 1);;

let rec roman l = function
	| 0 -> ""
	| x ->
		let fs (a, b) = a
		and sc (c, d) = d
		in
			let rec loop nowx = function
				| [] -> (0, "none")
				| v :: _ when (nowx >= fs v) -> v
				| _ :: rest -> loop nowx rest
			in
				let res = loop x l
				in
					sc res ^ roman l (x - fs res);;

roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984;;

let rec nested_length l =
	let rec list_length = function
		| [] -> 0
		| _ :: rest -> 1 + list_length rest
	in
		match l with
		| [] -> 0
		| nowlist :: rest -> list_length nowlist + nested_length rest;;

nested_length [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;

let rec concat = function
	| [] -> []
	| v :: rest -> v @ concat rest;;

concat [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;

let rec zip l1 l2 =
	match l1 with
	| [] -> []
	| v :: rest ->
	(
		match l2 with
		| [] -> []
		| v2 :: rest2 -> (v, v2) :: zip rest rest2
	);;

zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] [true; true; false; true; false; true; false; false; false; true];;

let unzip l =
	let appendTuple (a, b) c =
		(a :: fst c, b :: snd c)
	in
		let rec makeList = function
			| [] -> ([], [])
			| v :: rest -> appendTuple v (makeList rest)
		in
			makeList l;;

unzip (zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11] [true; true; false; true; false; true; false; false; false; true]);;

let rec filter p = function
	| [] -> []
	| v :: rest when p v = true -> v :: filter p rest
	| v :: rest -> filter p rest;;

let is_positive x = (x > 0);;

filter is_positive [-9; 0; 2; 5; -3];;

let rec length = function
	| [] -> 0
	| v :: rest -> 1 + length rest;;

filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]];;

let rec take n = function
	| [] -> []
	| _ :: _ when n = 0 -> []
	| v :: rest -> v :: (take (n - 1) rest);;

let ten_to_zero = downto1 10;;

take 8 ten_to_zero;;

let rec drop n = function
	| [] -> []
	| v :: rest when n <= 0 -> v :: (drop (n - 1) rest)
	| _ :: rest -> drop (n-1) rest;;

let max_list l =
	let rec loop res list =
		match list with
		| [] -> res
		| v :: rest -> max (loop v rest) v
	in
		loop 0 l;;

let rec mem a = function
	| [] -> false
	| v :: rest -> (a = v) || mem a rest;;

mem 5 [1;2;3;4;5];;

(*
	集合に同じ要素はないから適当に書いちゃった
*)

let rec intersect s1 s2 =
	match s1 with
	| [] -> []
	| v :: rest when mem v s2 -> v :: intersect rest s2
	| _ :: rest -> intersect rest s2;;

intersect [1;2;3] [2;3;4];;

let rec union s1 s2 =
	match s1 with
	| [] -> s2
	| v :: rest when not (mem v s2) -> v :: union rest s2
	| _ :: rest -> union rest s2;;

union [1;2;3] [2;3;4];;

let rec diff s1 s2 =
	match s1 with
	| [] -> []
	| v :: rest when not (mem v s2) -> v :: diff rest s2
	| _ :: rest -> diff rest s2;;

diff [1;2;3;4;5] [2;3;4];;

let rec map f = function
	| [] -> []
	| v :: rest -> f v :: map f rest;;

(*

	map (fun x -> f (g x)) l

*)

let rec fold_right f l e =
	match l with
	| [] -> e
	| v :: l' -> f (v) (fold_right f l' e);;

let concat l =
	fold_right (@) l [];;

concat [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;

let forall p l =
	fold_right (fun x y -> p (x) && y) l true;;

let exists p l =
	fold_right (fun x y -> p (x) || y) l false;;

let nextrand seed =
	let a = 16807.0 and m = 2147483647.0 in
		let t = a *. seed in
			t -. m *. floor (t /. m);;
let rec randlist n seed tail =
	if n = 0 then
		(seed, tail)
	else
		randlist (n - 1) (nextrand seed) (seed::tail);;

let rec quick_sort res = function
	| [] -> res
	| [x] -> x :: res
	| pivot :: rest ->
		let rec partition left right = function
			| [] -> quick_sort (pivot :: quick_sort res right) left
			| v :: vs -> if pivot < v then partition left (v :: right) vs else partition (v :: left) right vs
		in
			partition [] [] rest;;

quick_sort [] (snd (randlist 10 1.0 []));;

let squares r =
	let rec loop x y =
		if x * x > r then
			[]
		else if y > x then
			loop (x + 1) 0
		else if (x * x + y * y) = r then
			(x, y) :: loop x (y + 1)
		else
			loop x (y + 1)
	in
		loop 0 0;;

let rec list_length = function
	| [] -> 0
	| _ :: rest -> 1 + list_length rest;;

list_length (squares 48612265);;

let rec map f = function
	| [] -> []
	| v :: rest -> f v :: map f rest;;

let rec fold_right f l e =
	match l with
	| [] -> e
	| v :: l' -> f (v) (fold_right f l' e);;

let map2 f l =
	fold_right (fun x y -> f x :: y) l []
