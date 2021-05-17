type figure =
	| Point
	| Circle of int
	| Rectangle of int * int
	| Square of int;;

let rec similar x y =
	match (x, y) with
	| (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
	| (Rectangle (l1, l2), Rectangle (l3, l4)) -> (l3 * l2 - l4 * l1) = 0
	| (Rectangle (l1, l2), Square l3) -> (l1 = l3) && (l2 = l3)
	| (Square (l1), Rectangle (l2, l3)) -> similar (Rectangle (l2, l3)) (Square (l1))
	| _ -> false;;

type 'a with_location = {loc_x : float; loc_y: float; body: 'a};;

let a = {loc_x = 2.; loc_y = 3.; body = Circle 1};;
let b = {loc_x = 2.; loc_y = 2.; body = Rectangle (2, 2)};;

let rec overlap (x) (y) =
	let pow2 x =
		x *. x
	in
	let euclideanDistance x1 x2 y1 y2 =
		sqrt (pow2 (x1 -. x2) +. pow2 (y1 -. y2))
	in
	match (a.body, b.body) with
	| (Point, Point) ->  (a.loc_x = b.loc_x && a.loc_y = b.loc_y)
	| (Point, Circle r) ->  (euclideanDistance a.loc_x b.loc_x a.loc_y b.loc_y <= float_of_int r)
	| (Circle _, Point) -> overlap b a
	| (Circle r1, Circle r2) ->  (euclideanDistance a.loc_x b.loc_x a.loc_y b.loc_y <= float_of_int (r1 + r2) )
	| (Rectangle (l1, l2), Circle r) ->
		let tmpx = (float_of_int l1 /. 2.) and tmpy = (float_of_int l2 /. 2.)
		in
		if (a.loc_x -. tmpx -. float_of_int r <= b.loc_x) && (b.loc_x <= a.loc_x +. tmpx +. float_of_int r) && (a.loc_y -. tmpy <= b.loc_y) && (b.loc_y <= a.loc_y +. tmpy) then
			true
		else if (a.loc_y -. tmpy -. float_of_int r <= b.loc_y) && (b.loc_y <= a.loc_y +. tmpy +. float_of_int r) && (a.loc_x -. tmpx <= b.loc_x) && (b.loc_x <= a.loc_x +. tmpx) then
			true
		else
			let rec loop l =
				match l with
				| [] -> false
				| (x, y) :: rest -> overlap {b with loc_x = x; loc_y = y} b || loop rest
			in
			loop [(a.loc_x -. tmpx, a.loc_y -. tmpy); (a.loc_x -. tmpx, a.loc_y +. tmpy); (a.loc_x +. tmpx, a.loc_y -. tmpy); (a.loc_x +. tmpx, a.loc_y +. tmpy)]
	| (Circle r, Rectangle (l1, l2)) ->  overlap b a
	| (Square l1, Circle r) -> overlap {a with body = Rectangle (l1, l1)} b
	| (Circle r, Square l1) -> overlap b a
	| (Point, Rectangle (l1, l2)) ->
		let tmpx = (float_of_int l1 /. 2.) and tmpy = (float_of_int l2 /. 2.)
		in
		(b.loc_x -. tmpx <= a.loc_x) && (a.loc_x <= b.loc_x +. tmpx) && (b.loc_y -. tmpy <= a.loc_y) && (a.loc_y <= b.loc_y +. tmpy)
	| (Rectangle (l1, l2), Point) -> overlap b a
	| (Rectangle (l1, l2), Rectangle (l3, l4)) ->
		let tmpx = (float_of_int l1 /. 2.) and tmpy = (float_of_int l2 /. 2.)
		in
		let listOfPoint = [(a.loc_x -. tmpx, a.loc_y -. tmpy); (a.loc_x -. tmpx, a.loc_y +. tmpy); (a.loc_x +. tmpx, a.loc_y -. tmpy); (a.loc_x +. tmpx, a.loc_y +. tmpy)]
		in
		let rec loop l =
			match l with
			| [] -> false
			| (x, y) :: rest -> overlap {loc_x = x; loc_y = y; body = Point} b || loop rest
		in
		loop listOfPoint
	| (Rectangle (l1, l2), Square l3) -> overlap a {b with body = Rectangle (l3, l3)}
	| (Square l1, Rectangle (l2, l3)) -> overlap b a
	| (Square l1, Square l2) -> overlap {a with body = Rectangle (l1, l1)} {b with body = Rectangle (l2, l2)}
	| (Point, Square l1) -> overlap a {b with body = Rectangle (l1, l1)}
	| (Square l1, Point) -> overlap b a

type nat = Zero | OneMoreThan of nat;;

let rec add m n =
	match m with
	| Zero -> n
	| OneMoreThan m' -> OneMoreThan (add m' n);;

let rec loop x =
	match x with
	| Zero -> 0
	| OneMoreThan m' -> 1 + loop m';;

type natseq = Cons of nat * (nat -> natseq);;
let rec nextNat x = Cons (add x (OneMoreThan Zero), nextNat);;

let nthNat n =
	if n = 0 then
		Zero
	else
		let rec nthNat' n (Cons (x, f)) =
			if n = 1 then
				x
			else
				nthNat' (n-1) (f x)
		in
		nthNat' n (nextNat Zero);;

let rec natToNum n =
	match n with
	| Zero -> 0
	| OneMoreThan n' -> 1 + natToNum n';;

let mul m n =
	let rec mul' m n res =
		match n with
		| Zero -> res
		| OneMoreThan n' -> mul' m n' (add m res)
	in
	mul' m n Zero;;

let monus m n =
	let rec monus' m n =
		match m with
		| Zero -> Zero
		| OneMoreThan m' ->
		(
			match n with
			| Zero -> m
			| OneMoreThan n' -> monus' m' n'
		)
	in
	monus' m n;;

let minus m n =
	let rec minus' m n =
		match m with
		| Zero ->
			if n = Zero then
				Some Zero
			else
				None
		| OneMoreThan m' ->
		(
			match n with
			| Zero -> Some m
			| OneMoreThan n' -> minus' m' n'
		)
	in
	minus' m n;;

minus (nthNat 5) (nthNat 3);;

(* 見やすさのために真ん中を要素にしてみちゃったりした *)
type 'a tree = Lf | Br of 'a tree * 'a * 'a tree;;

let rec comptree x n =
	match n with
	| 0 -> Lf
	| n' ->
		let subtree = comptree x (n - 1)
		in
		Br (subtree, x, subtree);;

let comptree' n =
	let rec comptree'' x n =
		match n with
		| 0 -> Lf
		| n' ->
			Br ((comptree'' (2 * x + 1) (n - 1)), x ,(comptree'' (2 * x) (n - 1)))
	in
	comptree'' 1 n;;
