let nextrand seed =
	let a = 16807.0 and m = 2147483647.0 in
		let t = a *. seed in
			t -. m *. floor (t /. m);;
let rec randlist n seed tail =
	if n = 0 then
		(seed, tail)
	else
		randlist (n - 1) (nextrand seed) (seed::tail);;

let rec insert x = function
	| [] -> [x]
	| y :: rest when x < y -> x :: (y :: rest)
	| y :: rest -> y :: (insert x rest);;

insert 18.5 [-2.2; 9.1; 31.8];;

let rec insertion_sort = function
	| [] -> []
	| v :: rest -> insert v (insertion_sort rest);;

let rec quick_sort = function
	| [] -> []
	| pivot :: rest ->
		let rec partition left right = function
			| [] -> (left, right)
			| v :: vs -> if pivot < v then partition left (v::right) vs else partition (v::left) right vs
		in
			let (left, right) = partition [] [] rest in
				quick_sort left @ (pivot ::quick_sort right);;

let rec quick_sort = function
	| ([] | [_]) as l -> l
	| pivot :: rest ->
		let rec partition left right = function
			| [] -> (quick_sort left) @ (pivot :: quick_sort right)
			| v :: vs -> if pivot < v then partition left (v :: right) vs else partition (v :: left) right vs
		in
			partition [] [] rest;;

quick_sort (snd (randlist 10 1.0 []));;

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

(* snd (randlist 10 1.0 []);; *)
