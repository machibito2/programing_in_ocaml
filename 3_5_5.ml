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
