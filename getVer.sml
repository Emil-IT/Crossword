(* getVer' (l, n)
	TYPE: 'a list list * int -> 'a list
	PRE: All the lists in l are of the same length
	POST: The element on the position n in every list in l, put into a list.
	VARIANT: Size n
	EXAMPLE: getVer' ([[1,2,3,4], [5,6,7,8], [9,10,11,12]], 4) = 
		[4, 8, 12]
*)

fun getVer' ([], n) = []
  | getVer' (l, n) = List.drop((List.take((hd(l)), n)), n-1) @ getVer' (tl(l), n)
 
 (* getVer (l, n)
	TYPE: 'a list list * int -> 'a list list
	PRE: All the lists in l are of the same length
	POST: All elements up to the nth element in every list in l, put into individual lists.
	VARIANT: Size n
	EXAMPLE: getVer ([[1,2,3,4], [5,6,7,8], [9,10,11,12]], 4) = 
		[[4, 8, 12], [3, 7, 11], [2, 6, 10], [1, 5, 9]]
*)
fun getVer (l, 0) = []
  | getVer (l, n)  = rev(rev(getVer' (l, n))) :: getVer (l, n-1)