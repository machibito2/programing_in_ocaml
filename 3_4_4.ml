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

