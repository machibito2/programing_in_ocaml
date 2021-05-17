let dollar_to_yen x =
	int_of_float (floor (x *. 114.32));;
dollar_to_yen (1.5);;

let yen_to_dollar x =
	(floor (float_of_int (x) /. 114.32 *. 100.) /. 100.);;
yen_to_dollar (115);;

let dollar_to_dollar x =
	let yen = dollar_to_yen x in
		string_of_float (x) ^ " dollars are " ^ string_of_int (yen) ^ " yen.";;
dollar_to_dollar (1.);;

let capitalize c =
	if ('a' <= c && c <= 'z') then
		char_of_int(int_of_char(c) + int_of_char('A') - int_of_char('a'))
	else c;;

capitalize ('h');;

(*
	if b1 then b2 else false;;
	if b1 then true else b2;;

	not (not b1 || not b2)
	not (not b1 && not b2)
 *)

(* let geo_mean (x, y) =
	sqrt (x *. y);;
geo_mean (2. , 2.);; *)

let bmi (name, height, weight) =
	let status =
		let bmi_value =
			weight /. (height *. height)
		in
			if bmi_value < 18.5 then
				"やせ"
			else
				if bmi_value < 25. then
					"標準"
				else
					if bmi_value < 30. then
						"肥満"
					else
						"高度肥満"
	in
		name ^ "さんは" ^ status ^ "です";;
bmi ("ken", 1.81, 75.);;

let rec pown x n =
	if n = 1 then x
	else x * pown x (n-1);;

let rec pow x n =
	if n = 1 then x
	else
		if (n mod 2) = 0 then
			let tmp = pow x (n/2) in
				tmp * tmp
		else
			x * pow x (n - 1);;

let rec iterpow (i, res, x, n) =
	if i = n then
		res
	else
		iterpow (i + 1, res * x, x, n);;

let rec gcd m n =
	if m = n then
		m
	else
		gcd (n-m) m;;

let rec comb m n =
	if m = 0 || m = n then
		1
	else
		comb m (n-1) + comb (m-1) (n-1);;

let rec iterfib (i, f1, f2, n) =
	if i = n then
		f1
	else
		iterfib (i + 1, f2, f1 + f2, n);;

let rec max_ascii (s, res, i) =
	if i >= String.length s then
		res
	else
		let ma =
			if s.[i] > res then
				s.[i]
			else
				res
		in
			max_ascii(s, ma, i + 1);;

let rec pow x n =
	if n = 0 then
		1
	else
		if (n mod 2) = 0 then
			let tmp = pow x (n/2) in
				tmp * tmp
		else
			x * pow x (n-1);;

let cube = (fun x -> pow x 3);;

let integral f a b =
	let delta = 0.1e-3 in
		let rec loop i =
			if i < b then
				let daikei g c =
					((g c +. g (c +. delta)) *. delta) /. 2.0
				in
				daikei f i +. loop (i +. delta)
			else
				0.0
		in
		loop a;;

let fir a b c =
	a + b + c;;

let sec a (b : int) : int =
	a b;;

let thi a : int =
	a 1 2;;
