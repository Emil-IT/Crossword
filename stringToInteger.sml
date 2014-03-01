
(* clToInt
TYPE: char list -> int list
PRE: True
POST: A list of the characters in cl converted to a number equal to that characters order of appearance. 
EXAMPLE: 	clToInt [#"H", #"E", #"J", #"H", #"U", #"R", #"G", #"A", #"R"] 
			= [1, 2, 3, 1, 4, 5, 6, 7, 5]
*)

fun clToInt cl = 
let
val acc = []

	(* chrToInt
		TYPE: char list * (char * int) list * int -> int list
		PRE: n < 0
		POST: A list with the characters in cl exchanged to n, n+1 and so on, set by their order of appearance in acc.
		VARIANT: length cl
		EXAMPLE: 	chrToInt ([#"H", #"E", #"J", #"H"], [], 1)
					= [1, 2, 3, 1]
		*)
	
	fun chrToInt ([], acc, n) = []
	| chrToInt (cl, acc, n) =
	let
	
	(* chrToInt'
		TYPE: char list * (char * int) list * int -> int list
		PRE: 
		POST: A list with the characters in (hd::tl) exchanged to n, n+1 and so on, set by their order of appearance in (ch, i)::l.
		VARIANT:  length (hd::tl)
		EXAMPLE: 	chrToInt' ([#"H", #"E", #"J", #"H"], [], 4) 
					= [4, 5, 6, 4]
		*)
		
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

(* slTocl
TYPE: string list -> char list
PRE: 
POST: A list of the characters in the string in (hd::tl).
VARIANT: length of (hd::tl)
EXAMPLE: 	slTocl ["HEJ"] = [#"H", #"E", #"J"]
*)

fun slTocl [] = []
  | slTocl (hd::tl) = explode hd @ slTocl (tl)

(* sllTocll
TYPE: string list list -> char list
PRE: 
POST: A list of all the characters in (hd::tl)
VARIANT: length (hd::tl)
EXAMPLE: 	sllTocl [["FUNKY"], ["TOWN"]] 
			= [#"F", #"U", #"N", #"K", #"Y", #"T", #"O", #"W", #"N"]
*)
  
fun sllTocl [] = []
  | sllTocl (hd::tl) = slTocl (hd) @ sllTocl (tl)

  (* take (l, n)
TYPE: 'a list * int -> 'a list
PRE: True
POST: The n first elements inserted into ll if n > 0, else l.
VARIANT: size n
EXAMPLE: 	take ([1,2,3,4,5,6], 3) = [1, 2, 3]
*)
 
  
fun take ([], n, ll) = ll
  | take (_, 0, ll) = ll
  | take (hd::tl, n, ll) = take (tl, n-1, hd::ll)

(* drop (l, n)
TYPE: 'a list * int -> 'a list
PRE: True
POST: A list without the n first elements, an empty list if n < 0.
VARIANT: size n
EXAMPLE: 	drop ([1,2,3,4,5,6], 3) = [4, 5, 6]
*)
 
  
fun drop ([], _) = []
  | drop (hd::tl, 0) = hd::tl
  | drop (hd::tl, n) = drop (tl, n-1)

(* divide (l, n)
TYPE: 'a list * int -> 'a list list
PRE: n > 0
POST: A list with the elements in l sorted into lists of length n.
VARIANT: size n
EXAMPLE: 	divide ([1,2,3,4,5,6], 2) = [[1, 2], [3, 4], [5, 6]]
			divide ([1,2,3,4,5,6], 500) = [[1, 2, 3, 4, 5, 6]]
*)
 
fun divide ([], _) = []
  | divide (l, n) = 
	rev (take (l, n, [])) :: divide(drop(l, n), n) 


(* stringToInteger s
TYPE: string list list -> int list list
POST: The string s must consist of valid characters.
PRE: A list of lists with the characters in the lists in s, converted to a number equal to that characters order of appearance. 
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
  

  
  
  
