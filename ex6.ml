(* 6.1 *)

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

(* 6.2 *)

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

(* 6.3 *)

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

(* 6.4 *)

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

(* 6.5 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x n =
	match n with
	| 0 -> Lf
	| n' ->
		let subtree = comptree x (n - 1)
		in
		Br (x, subtree, subtree);;

let comptree' n =
	let rec comptree'' x n =
		match n with
		| 0 -> Lf
		| n' ->
			Br (x, (comptree'' (2 * x + 1) (n - 1)) ,(comptree'' (2 * x) (n - 1)))
	in
	comptree'' 1 n;;

(* 6.6 *)

let rec preord res = function
	| Lf -> res
	| Br (x, left, right) -> x :: (preord (preord res right) left);;

let rec inord res = function
	| Lf -> res
	| Br (x, left, right) -> inord (x :: inord (res) right) left;;

let rec postord res = function
	| Lf -> res
	| Br (x, left, right) -> postord (postord (x :: res) right) left;;

(* 6.7 *)

let comptree = Br(1,
					Br(2,
						Br(4, Lf, Lf),
						Br(5, Lf, Lf)),
					Br(3,
						Br(6, Lf, Lf),
						Br(7, Lf, Lf)));;

let rec fold_left f e l =
	match l with
	| [] -> e
	| v :: rest -> fold_left f (f e v) rest;;

let reverse l = fold_left (fun x y -> y :: x) [] l;;

let rec reflect t =
	match t with
	| Lf -> Lf
	| Br (x, left, right) -> Br (x, reflect right, reflect left);;

let rec inorder = function
	Lf -> []
	| Br (x, left, right) -> (inorder left) @ (x::inorder right);;

preord [] (reflect comptree) = reverse (postord [] comptree);;
inord [] (reflect comptree) = reverse (inord [] comptree);;
postord [] (reflect comptree) = reverse (preord [] comptree);;

(* 6.8 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

let rec map f l =
	match l with
	| [] -> []
	| v :: rest -> (f v) :: map f rest;;

let rec tree_of_rtree = function
	| RLf -> Br (None, Lf, Lf)
	| RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
	| [] -> Lf
	| rtree :: rest -> let Br (a, left, Lf) = tree_of_rtree rtree
	in
	Br (a, left, tree_of_rtreelist rest);;

let rtree = RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])]);;

(* let rec testMigi res = function
	| Lf -> res
	| Br (a, left, right) -> RBr(a, testMigi [] right) :: res;; *)

(* 6.8 *)
(* 場合分けして再帰するとなんかうまくいく *)

let rtree_of_tree t =
	let rec rtree_of_tree' = function
		| Lf -> [RLf]
		| Br (None, Lf, Lf) -> [RLf]
		| Br (None, Lf, right) -> RLf :: (rtree_of_tree' right)
		| Br (Some a, left, Lf) -> [RBr (a, rtree_of_tree' left)]
		| Br (Some a, left, right) -> RBr (a, rtree_of_tree' left) :: (rtree_of_tree' right)
	in
	let ans = rtree_of_tree' t
	in
	match ans with
	| [ans] -> ans;;


let rtree = RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])]);;

rtree_of_tree (tree_of_rtree rtree) = rtree;;

(* 6.9 *)
(* 適当にスタックでやった *)
(* ちゃんとやれば木になるんじゃないですか?眠いので寝ます *)

type token = PCDATA of string | Open of string | Close of string;;

let token_list = [Open "a"; Close "a"; Open "b"; Close "b"; Open "c"; PCDATA "Hello"; Close "c";];;

let rec makeXML stack res = function
	| [] when stack = [] -> res
	| (Close a) :: rest ->
		(
			match stack with
			| v :: rrest when a = v -> makeXML rrest (res ^ ("</" ^ a ^ ">")) rest
			| _ -> "dame"
		)
	| (Open a) :: rest -> makeXML (a :: stack) (res ^ ("<" ^ a ^ ">")) rest
	| (PCDATA a) :: rest when stack <> [] -> makeXML stack (res ^ a) rest
	| _ -> "dame";;

(* 6.10 *)

type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let rec eval = function
	| Mul (left, right) -> (eval left) * (eval right)
	| Add (left, right) -> (eval left) + (eval right)
	| Const c -> c;;

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;
let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;
eval exp;;

(* 6.11 *)
(* なんとかしてstring_of_arithの最外カッコを消すとカッコが減っていい感じだけどめんどうだからやらない *)

let rec string_of_arith = function
	| Mul (left, right) -> "(" ^ (string_of_arith left) ^ "*" ^ (string_of_arith right) ^ ")"
	| Add (left, right) -> "(" ^ (string_of_arith left) ^ "+" ^ (string_of_arith right) ^ ")"
	| Const c -> string_of_int c;;

let rec expand = function
	| Mul (Add (lleft, lright), Add (rleft, rright)) -> Add (Add (Mul (lleft, rleft), Mul (lleft, rright)), Add (Mul (lright, rleft), Mul (lright, rright)))
	| Mul (left, Add (rleft, rright)) -> Add (Mul (left, rleft), Mul (left, rright))
	| Mul (Add (lleft, lright), right) -> Add (Mul (lleft, right), Mul (lright, right))
	| Mul (left, right) -> Mul (expand (left), expand (right))
	| Add (left, right) -> Add (expand (left), expand (right))
	| Const c -> Const c;;

(* 6.12 *)
(* ひまだったしチェックするやつをいっぱい書いた *)
(* permutationは引用した *)

type bst = BSTLf | BSTBr of int * bst * bst;;

let rec bstInsert a = function
	| BSTLf -> BSTBr (a, BSTLf, BSTLf)
	| BSTBr (v, left, right) ->
		if v < a then
			BSTBr (v, bstInsert a left, right)
		else
			BSTBr (v, left, bstInsert a right);;

let rec bstInsertList res = function
	| [] -> res
	| v :: rest -> bstInsertList (bstInsert v res) rest;;

let rec member a = function
	| [] -> ([a], false)
	| v :: rest ->
	(
		match v with
		| (left, right) ->
			if a = right then
				(a :: left, true)
			else
				member a rest
	);;

let rec numberOfMember = function
	| [] -> 0
	| v :: rest -> 1 + numberOfMember rest;;

let rec check res = function
	| [] -> res
	| v :: rest ->
	(
		let ans = bstInsertList BSTLf v
		in
		let rec loop a = function
			| [] -> [([a], ans)]
			| (left, right) :: rest ->
				if ans = right then
					(a :: left, right) :: rest
				else
					(left, right) :: loop a rest
		in
		check (loop v res) rest
	);;

let rec interleave x lst =
	match lst with
	| [] -> [[x]]
	| hd::tl -> (x::lst) :: (List.map (fun y -> hd::y) (interleave x tl))

let rec permutations lst =
	match lst with
	| hd::tl -> List.concat (List.map (interleave hd) (permutations tl))
	| _ -> [lst];;

let a = check [] (permutations [1;2;3;4]);;

(*
 [[[1; 2; 3; 4]];
 [[2; 3; 4; 1]; [2; 3; 1; 4]; [2; 1; 3; 4]];
 [[1; 3; 4; 2]; [1; 3; 2; 4]];
 [[3; 4; 1; 2]; [3; 1; 4; 2]; [3; 1; 2; 4]];
 [[3; 4; 2; 1]; [3; 2; 4; 1]; [3; 2; 1; 4]];
 [[1; 2; 4; 3]];
 [[2; 4; 3; 1]; [2; 4; 1; 3]; [2; 1; 4; 3]];
 [[1; 4; 2; 3]];
 [[4; 1; 2; 3]];
 [[4; 2; 3; 1]; [4; 2; 1; 3]];
 [[1; 4; 3; 2]];
 [[4; 1; 3; 2]];
 [[4; 3; 1; 2]];
 [[4; 3; 2; 1]]]
*)

(*
 [
 BSTBr (1, BSTBr (2, BSTBr (3, BSTBr (4, BSTLf, BSTLf), BSTLf), BSTLf),BSTLf);
 BSTBr (2, BSTBr (3, BSTBr (4, BSTLf, BSTLf), BSTLf),BSTBr (1, BSTLf, BSTLf));
 BSTBr (1, BSTBr (3, BSTBr (4, BSTLf, BSTLf), BSTBr (2, BSTLf, BSTLf)),BSTLf);
 BSTBr (3, BSTBr (4, BSTLf, BSTLf),BSTBr (1, BSTBr (2, BSTLf, BSTLf), BSTLf));
 BSTBr (3, BSTBr (4, BSTLf, BSTLf),BSTBr (2, BSTLf, BSTBr (1, BSTLf, BSTLf)));
 BSTBr (1, BSTBr (2, BSTBr (4, BSTLf, BSTBr (3, BSTLf, BSTLf)), BSTLf),BSTLf);
 BSTBr (2, BSTBr (4, BSTLf, BSTBr (3, BSTLf, BSTLf)),BSTBr (1, BSTLf, BSTLf));
 BSTBr (1, BSTBr (4, BSTLf, BSTBr (2, BSTBr (3, BSTLf, BSTLf), BSTLf)),BSTLf);
 BSTBr (4, BSTLf,BSTBr (1, BSTBr (2, BSTBr (3, BSTLf, BSTLf), BSTLf), BSTLf));
 BSTBr (4, BSTLf,BSTBr (2, BSTBr (3, BSTLf, BSTLf), BSTBr (1, BSTLf, BSTLf)));
 BSTBr (1, BSTBr (4, BSTLf, BSTBr (3, BSTLf, BSTBr (2, BSTLf, BSTLf))),BSTLf);
 BSTBr (4, BSTLf,BSTBr (1, BSTBr (3, BSTLf, BSTBr (2, BSTLf, BSTLf)), BSTLf));
 BSTBr (4, BSTLf,BSTBr (3, BSTLf, BSTBr (1, BSTBr (2, BSTLf, BSTLf), BSTLf)));
 BSTBr (4, BSTLf,BSTBr (3, BSTLf, BSTBr (2, BSTLf, BSTBr (1, BSTLf, BSTLf))))]
*)
