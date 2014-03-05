
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
		if ord hd = 48 then 0 :: (chrToInt (tl, acc, n))
		else if ord hd = ord ch then i :: (chrToInt (tl, acc, n))
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

(* divide (l, n)
TYPE: 'a list * int -> 'a list list
PRE: 0 < n <= length l
POST: A list with the elements in l sorted into lists of length n.
VARIANT: size n
EXAMPLE: 	divide ([1,2,3,4,5,6], 2) = [[1, 2], [3, 4], [5, 6]]
			divide ([1,2,3,4,5,6], 500) = [[1, 2, 3, 4, 5, 6]]
*)
 
fun divide ([], _) = []
  | divide (l, n) = 
	List.take (l, n) :: divide(List.drop(l, n), n) 


(* stringToInteger s
TYPE: string list list -> int list list
POST: The string s must consist of valid characters.
PRE: A list of lists with the characters in the lists in s, converted to a number equal to that characters order of appearance. 
EXAMPLE: stringToInteger [["HEJ"], ["HUR"], ["GAR"], ["DET"]] =
		 [[1, 2, 3], [1, 4, 5], [6, 7, 5], [8, 2, 9]]

*)  
  
fun stringToInt s =
	let 
	    fun stringToInt' ([], _, _, acc) = rev acc
	      | stringToInt' (s::sl, ml, n, acc) = 
		let
		    fun clToInt ([], ml : (char*int) list, n, acc) = (ml, n, rev acc)
		      | clToInt (c::cl, [], n, acc) = clToInt(cl, [(c,n)], n+1, n::acc)
		      | clToInt (c::cl, ml, n, acc) = 
			let 
			    fun getNr (c1, []) = NONE
			      | getNr (c1, (c2, n)::tl) = 
				if c1 = c2 then 
				    SOME n 
				else 
				    getNr(c1, tl)
			    val result = getNr(c, ml)
			in
			    if isSome result then
				clToInt(cl, ml, n, (valOf result)::acc)
			    else
				clToInt(cl, (c, n)::ml, n+1, n::acc)
			end
		    val (ml, n, word) = clToInt(explode s, ml, n, [])
		in
		    stringToInt'(sl, ml, n, word::acc)
		end
	in
	    stringToInt'(s, [], 1, [])
	end
			    
		
	in
	    stringToInt' (map explode s, [], [])
	end;

val test = [["HEJ"], ["HUR"], ["GAR"], ["DET"]];
  

  
  
  
