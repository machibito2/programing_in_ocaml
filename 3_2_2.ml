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
