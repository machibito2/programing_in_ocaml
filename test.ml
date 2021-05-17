let sum_list3 [x; y; z] = x + y + z;;

let rec sum_list l =
	match l with
	| [] -> 0
	| n :: rest -> n + (sum_list rest);;

let rec sum_list = function
	| [] -> 0
	| n :: rest -> n + (sum_list rest);;

let rec length = function
	| [] -> 0
	| _ :: rest -> 1 + length rest;;

let rec append l1 l2 =
	match l1 with
	| [] -> l2
	| v :: l1' -> v :: (append l1' l2);;

let rec reverse l =
	match l with
	| [] -> []
	| v :: l' -> append (reverse (l')) [v];;

let rec reverse l =
	let rec tmp l1 l2 =
		match l1 with
		| [] -> l2
		| v :: l1' -> tmp l1' (v :: l2)
	in
	tmp l [];;

reverse [1;2;3];;

let rec map f l =
	match l with
	| [] -> []
	| v :: l' -> (f v) :: map f l';;

map (fun x -> x * 2) [4; 91; 0; -34];;

let rec forall f l =
	match l with
	| [] -> true
	| v :: l' ->
		if f v = true then
			forall f l'
		else
			false;;

let rec forall p = function
	| [] -> true
	| v :: rest -> (p v) && forall p rest;;

let rec exists p = function
	| [] -> false
	| v :: rest -> (p v) || (exists p rest);;

let rec fold_right f l e =
	match l with
	| [] -> e
	| v :: l' -> f (v) (fold_right f l' e);;

(*
	fold_right f l e
	-> f v (fold_right f l e)
	-> f v (f v (fold_right f l e))
	-> f v (f v e)
*)

fold_right (fun x y -> x + y) [3; 5; 7] 0;;

let rec fold_left f l e =
	match l with
	| [] -> e
	| v :: l' -> fold_left f l' (f e v);;

(*

	fold_left f l e
	-> fold_left f l (f e v)
	-> fold_left f l (f (f e v) v)
	-> (f (f e v) v)


*)

fold_left (fun x y -> y :: x) [1;2;3] [];;

let length l = fold_right (fun x y -> 1 + y) l 0;;

length [1; 2; 3];;

let rec ntn n l =
	if n = 1 then
		let a :: _ = l in a
	else
		let _ :: rest = l in ntn (n - 1) rest;;

let rec nth n l =
	match n with
	| 1 -> let a :: _ = l in a
	| _ -> let _ :: rest = l in nth (n-1) rest;;

let rec nth n l =
	match (n, l) with
	| (1, a :: _) -> a
	| (_, _ :: rest) -> nth (n-1) rest;;

let rec nth n l =
	match (n, l) with
	| (l, a :: _) -> a
	| (n', _ :: rest) when n' > 0 -> nth (n - 1) rest;;

let f = function
	| [] -> 0
	| x :: rest when x > 0 -> 1
	| x :: rest when x <= 0 -> 2;;

let rec assoc a = function
	(a', b) :: rest -> if a = a' then b else assoc a rest;;

let rec assoc a = function
	| (a, b) :: rest -> b
	| _ :: rest -> assoc a rest;;
