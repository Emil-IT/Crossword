fun solve' l =
getFromList (hd (l)) [["A", "B"], ["AB", "BA"], ["ABA", "BAB", "AAA", "BBB", "BAA", "ABB"]] (ref[]) @ solve' (tl(l))
	
fun solve l = 
solve' (preprocess l)
