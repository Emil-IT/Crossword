
fun clToInt cl = 
let
val acc = []
	fun chrToInt ([], acc, n) = []
	| chrToInt (cl, acc, n) =
	let
		fun chrToInt' ([], _, _) = []
		| chrToInt' (hd::tl, [], n) = n :: chrToInt (tl, ((hd, n)::acc), n+1)
		| chrToInt' (hd::tl, (ch, i)::l, n) =
		if ord hd = ord ch then i :: (chrToInt (tl, acc, n))
		else chrToInt' (hd::tl, l, n)
	in
	chrToInt' (cl, acc, n)
	end

in
chrToInt (cl, acc, 1)
end;


fun slTocl [] = []
  | slTocl (hd::tl) = explode hd @ slTocl (tl)

fun sllTocl [] = []
  | sllTocl (hd::tl) = slTocl (hd) @ sllTocl (tl)

  
fun take ([], n, ll) = ll
  | take (_, 0, ll) = ll
  | take (hd::tl, n, ll) = take (tl, n-1, hd::ll)


fun drop ([], _) = []
  | drop (hd::tl, 0) = hd::tl
  | drop (hd::tl, n) = drop (tl, n-1)

 
fun divide ([], _) = []
  | divide (l, n) = 
rev (take (l, n, [])) :: divide(drop(l, n), n) 

(* stringToInteger s
TYPE: string list list -> int list list
POST:
PRE: The characters in the lists in s, converted to a number equal to that characters order of appearance. 
EXAMPLE: stringToInteger [["HEJ"], ["HUR"], ["GAR"], ["DET"]] =
		 [[1, 2, 3], [1, 4, 5], [6, 7, 5], [8, 2, 9]]

*)  
  
fun stringToInteger s =
	let 
		val n = size(hd (hd(s)))
	in
		divide(clToInt (sllTocl s), n)

	end;

val test = [["HEJ"], ["HUR"], ["GAR"], ["DET"]];
  

  
  
  
