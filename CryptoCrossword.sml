(* CryptoCrossword *)
use "dictionarys.sml";

(*
REPRESENTATION CONVENTION: The datatype represents data in the form of a list, containing lists with a mutable datatype as elements.
REPRESENTATION INVARIANT: All lists must be of the same length.
*)
datatype 'a cPuzzle = CP of 'a list list
val empty = CP([])

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
  | getVer (l, n)  = getVer' (l, n) :: getVer (l, n-1)

(*
 preprocess puzzle
 TYPE: int list list -> int list list
 PRE: every element in puzzle are of the same length
 POST: a list of all of the groups of non-zero numbers in a row (either following each other in a list or having the same position in following lists), and also excluding groups of length 1
 EXAMPLE: preprocess [[1,2,0],[0,3,4],[5,0,6]] = [[1, 2], [3, 4], [4, 6], [2, 3]]
 VARIANT: length puzzle
*)
fun preprocess puzzle = 
    let
        (*
        toHorizontal (puzzle, acc)
        TYPE: ''a list list * ''a list list -> ''a list list
        PRE: every element in puzzle are of the same length
        POST: a ''a list list where the elements of the each list is made up of the elements of the lists in puzzle with the same position
        EXAMPLE: toHorizontal ([[1,2,3], [4,5,6], [7,8,9]], []) = [[3, 6, 9], [2, 5, 8], [1, 4, 7]]
        VARIANT: length puzzle
        *)
	fun toHorizontal ([], acc) = acc
	  | toHorizontal (puzzle, acc) = 
	    let
                (*
                tailFold (a, b)
                TYPE: 'a list * 'a list list -> 'a list list
                PRE: true
                POST: b with the tail of a as head
                EXAMPLE: tailFold ([1,2,3], [[0], [1,2]]) = [[2, 3], [0], [1, 2]]
                *)
		fun tailFold ([], b) = b
		  | tailFold (a, b) = (tl a)::b

                (*
                headFold (a, b)
                TYPE: 'a list * 'a list -> 'a list
                PRE: true
                POST: b with the head of a as head
                EXAMPLE: headFold ([1,2,3], [4,5,6]) = [1, 4, 5, 6]: int list
                *)
		fun headFold ([], b) = b
		  | headFold (a, b) = (hd a)::b
		val nextList = foldr headFold [] puzzle
	    in
		toHorizontal (foldr tailFold [] puzzle, if nextList = [] then acc else nextList::acc)
	    end
	(*
         preprocess' puzzle
         TYPE: int list list -> int list list
         PRE: true
         POST: int list list with the elements of the lists being the groups of non-zero numbers in a row (and excluding groups of length 1) of the elements of the lists in puzzle
         EXAMPLE: preprocess' [[1,2,3],[0,1,2],[],[0,1,0]] = [[1, 2, 3], [1, 2]]
         VARIANT: length puzzle
	*)
	fun preprocess' [] = []
	  | preprocess' (p::puzzle) =
	    let
		(*
                 preprocess'' (list, acc)
                 TYPE: int list * int list -> int list list
                 PRE: true
                 POST: int list list with the elements being the groups of non-zero numbers in a row (and excluding groups of length 1)
                 EXAMPLE: preprocess'' ([1,2,0,3,4,0,1], []) = [[1, 2], [3, 4]]
                 VARIANT: length list
		*)
		fun preprocess'' ([], []) = []
		  | preprocess'' ([], [a]) = []
		  | preprocess'' ([], acc) = (rev acc)::[]
		  | preprocess'' (0::list, []) = preprocess'' (list, [])
		  | preprocess'' (0::list, [a]) = preprocess'' (list, [])
		  | preprocess'' (0::list, acc) = (rev acc)::(preprocess'' (list, [])) 
		  | preprocess'' (l::list, acc) = preprocess''(list, l::acc)

		val a = preprocess'' (p, [])
	    in
		a @ preprocess' puzzle
	    end
    in 
	preprocess' (puzzle@(toHorizontal (puzzle, [])))
    end
		

(*
getFromList 
TYPE: ''a list -> string list list -> (char * ''a) list ref -> char list option
PRE: 
POST:
EXAMPLE:
VARIANT:
*)
fun getFromList word nil _ = (NONE, [])
  | getFromList _ (nil::_) _ = (NONE, [])
  | getFromList word (dictionary as ((hd::tl)::t2)) ml =
    let 
	val testWord = explode hd
	val mlCopy = ref (!ml)

	fun wordOK nil nil ml = true
	  | wordOK (nrhd::nrtl) (whd::wtl) ml =
	    let
		fun isOK _ _ nil = true
		  | isOK l n ((ll, ln)::tl) = 
		    if l = ll then 
			if n = ln then true
			else false
		    else if n = ln then 
			false
		    else isOK l n tl
	    in 
		(isOK whd nrhd (!ml)) andalso (ml := ((whd, nrhd)::(!ml)) ;(wordOK nrtl wtl ml))
	    end
    in
	if (length testWord) = (length word) then
	    if  wordOK word testWord mlCopy then 
		(ml := (!mlCopy); (SOME testWord, [tl]))
	    else
		(mlCopy := (!ml); getFromList word [tl] mlCopy)
	else 
	    getFromList word t2 ml
    end


fun solve puzzle = 
    let
	fun solve' ([], dict, ml) = true
	  | solve' (puzzle as hd::tl, dict, ml) = 
	    let
		val result = getFromList hd dict ml
		val mlCopy = ref (!ml)
	    in
		if isSome (#1result) andalso solve' (tl, shortlist, mlCopy) then 
		    (ml := (!mlCopy); true)
		else
		    solve' (puzzle, #2result, ml)
	    end
	val ml = ref []
    in
	if solve'(preprocess puzzle, shortlist, ml) then 
	    SOME ml
	else
	    NONE
    end
