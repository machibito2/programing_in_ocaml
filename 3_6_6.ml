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
