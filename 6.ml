type student = {name : string; id : int};;
let st = {name = "ken"; id = 2};;

let st1 = {name = "taro"; id = 123456};;
let st2 = {id = 51; name = "ichiro"};;

let st3 = {st1 with id = 234567};;

let string_of_student {name = n; id = i} =
	n ^ "'s ID is " ^ string_of_int i;;

let name_of_student {name = n} = n;;

let string_of_student st = st.name ^ "'s ID is " ^ string_of_int st.id;;

type teacher =
	{
		tname : string;
		office : string;
		ext : int
	};;

type student_teacher =
	{
		s : student;
		t : teacher
	};;

let area_of_figure = function
	| ("point", _, _) -> 0
	| ("circle", r, _) -> r * r* 3
	| ("rectangle", x, y) -> x * y
	| ("square", x, _) -> x * x;;

type figure =
	| Point
	| Circle of int
	| Rectangle of int * int
	| Square of int;;

let c = Circle 3;;

let figs = [Point; Square 5; Rectangle (4, 5); c];;

let area_of_figure = function
	| Point -> 0
	| Circle r -> r * r * 3
	| Rectangle (x, y) -> x * y
	| Square x -> x * x;;

let similar x y =
	match (x, y) with
	| (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
	| (Rectangle (l1, l2), Rectangle (l3, l4)) -> (l3 * l2 - l4 * l1) = 0
	| _ -> false;;

type color = Black | Blue | Red | Magenta | Green | Cyan | Yellow | White;;

let reverse_color = function
	| Black -> White
	| Blue -> Yellow
	| Red -> Cyan
	| Magenta -> Green
	| Green -> Magenta
	| Cyan -> Red
	| Yellow -> Blue
	| White -> Black;;

type nat = Zero | OneMoreThan of nat;;

let rec add m n =
	match m with
	| Zero -> n
	| OneMoreThan m' -> OneMoreThan (add m' n);;

type intlist = INil | Icons of int * intlist;;

let add_to_list m n =
	Icons (m, n);;

add_to_list 6 (add_to_list 5 INil);;

type even = Zero | OMT_E of odd
	and odd = OMT_O of even;;

let rec o_plus_e (OMT_O e1) e2 = OMT_O (e_plus_e e1 e2)
	and e_plus_e e1 e2 =
		match e1 with
		| Zero -> e2
		| OMT_E o -> OMT_E (o_plus_e o e2);;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

type 'a with_location = {loc_x : float; loc_y: float; body: 'a};;
{loc_x = 0.0; loc_y = 0.0; body = Point};;

type ('a, 'b) list_with_tail = Nil of 'b | Cons of 'a * ('a, 'b) list_with_tail;;

let aa = Cons (2, Cons(3, Nil ("end")));;

let fact n =
	let rec fact' n =
		if n = 0 then
			1
		else n * fact' (n - 1)
	in
	if n < 0 then
		None
	else
		Some (fact' n);;

type ('a, 'b) sum = Left of 'a | Right of 'b;;

[Left 1; Right "foo"];;

(* 二分木は葉か要素の左右に二分木がついてるもの *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let chartree = Br ('a', Br ('b', Br ('d', Lf, Lf), Lf), Br ('c', Br ('e', Lf, Lf), Br ('f', Lf, Lf)));;

let rec size = function
	| Lf -> 0
	| Br (_, left, right) -> 1 + size left + size right;;

let rec depth = function
	| Lf -> 0
	| Br (_, left, right) -> 1 + max (depth left) (depth right);;

size chartree;;
depth chartree;;

(* 完全二分木 *)
let comptree = Br( 1, Br( 2, Br( 4, Lf, Lf), Br( 5, Lf, Lf)), Br( 3, Br( 6, Lf, Lf), Br( 7, Lf, Lf)));;

size comptree;;
depth comptree;;

let rec preorder = function
	| Lf -> []
	| Br (x, left, right) -> x :: (preorder left) @ (preorder right);;

let rec inorder = function
	| Lf -> []
	| Br (x, left, right) -> (inorder left) @ (x :: (preorder right));;

let rec postorder = function
	| Lf -> []
	| Br (x, left, right) -> (postorder left) @ (postorder right) @ [x];;


let rec preord res = function
	| Lf -> res
	| Br (x, left, right) -> x :: (preord (preord res right) left);;

let rec inord res = function
	| Lf -> res
	| Br (x, left, right) -> inord (x :: inord (res) right) left;;

let rec postord res = function
	| Lf -> res
	| Br (x, left, right) -> postord (postord (x :: res) right) left;;

preorder comptree;;
preord [] comptree;;
inorder comptree;;
inord [] comptree;;
postorder comptree;;
postord [] comptree;;

let rec member t x =
	match t with
	| Lf -> false
	| Br (y, left, right) ->
		if y = x then
			true
		else
			if y > x then
				member left x
			else
				member right x;;

let rec add t x =
	match t with
	| Lf -> Br (x, Lf, Lf)
	| (Br (y, left, right) as whole) when x = y -> whole
	| Br (y, left, right) when x < y -> Br (y, add left x, right)
	| Br (y, left, right) -> Br (y, left, add right x);;

type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;
let addressbook = XBr ("addressbook", [ XBr ("person", [ XBr ("name", [XLf (Some "Atsushi Igarashi")]); XBr ("tel", [XLf (Some "075-123-4567")])]); XBr ("person", [XLf None]); XBr ("person", [XLf None])]);;
let rec string_of_xml xmlIn =
	match xmlIn with
	| XBr (tag, xml_list) -> "<" ^ tag ^ ">" ^ string_of_xmllist xml_list ^ "</" ^ tag ^ ">"
	| XLf None -> ""
	| XLf (Some s) -> s
and string_of_xmllist xmlIn =
	match xmlIn with
	| [] -> ""
	| xml :: rest -> string_of_xml xml ^ string_of_xmllist rest;;
string_of_xml addressbook;;

let rec map f = function
	| [] -> []
	| v :: rest -> f v :: map f rest;;

let rec rosetree_of_tree = function
	| Lf -> RLf
	| Br (a, left, right) -> RBr (a, map rosetree_of_tree [left; right]);;

let rec tree_of_rosetree = function
	| RLf -> Lf
	| RBr (a, roseList) -> Br (Some a, tree_of_rosetreelist roseList, Lf)
and tree_of_rosetreelist = function
	| [] -> Lf
	| rtree :: rest -> let Br (a, left, Lf) = tree_of_rosetree rtree
	in
	Br (a, left, tree_of_rosetreelist rest);;

type intseq = Cons of int * (int -> intseq);;
let rec step1 x = Cons ((x + 1), step1);;
let rec step2 x = Cons ((x + 2), step2);;
let Cons(x1, f1) = step2 0;;
let rec step n x = Cons ((x + n), step (n + 1));;

let rec nthseq n (Cons(x, f)) =
	if n = 1 then
		x
	else
		nthseq (n-1) (f x);;

let rec fold_right f unit = function
	| [] -> unit
	| v :: rest -> f v (fold_right f unit rest);;

let rec fold_left f unit = function
	| [] -> unit
	| v :: rest -> fold_left f (f unit v) rest;;

let is_prime x =
	let rec is_divisible_from_2_to n =
		(n > 1) && ((x mod n == 0) || is_divisible_from_2_to (n-1))
	in
		not (is_divisible_from_2_to (x - 1));;

let rec next_prime x =
	if is_prime (x + 1) then
		x + 1
	else
		next_prime (x + 1);;

let rec prime_seq x =
	if is_prime (x + 1) then
		Cons(x + 1, prime_seq)
	else
		prime_seq (x + 1);;
