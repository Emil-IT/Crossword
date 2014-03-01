(* Emil Österberg, Jonas Karlsson, Robin Sund *)
(* CryptoCrossword *)

use "dictionarys.sml";

(*
REPRESENTATION CONVENTION: A cPuzzle is reprecented by a int list list, where each int list reprecents a row in the crossword. An empty square is reprecented by a 0.
REPRESENTATION INVARIANT: All lists must be of the same length.
*)
datatype cPuzzle = CP of int list list
val empty = CP([])


val testList = [["AB", "BA", "BB", "AC", "BC", "CC", "CA", "CB", "AD", "BD", "CD", "DD", "DA", "DB", "DC","AA"],["BAC", "BBC"]]

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
                POST: b with the tail of a as head. If a is the empty list then b.
                EXAMPLE: tailFold ([1,2,3], [[0], [1,2]]) = [[2, 3], [0], [1, 2]]
                *)
		fun tailFold ([], b) = b
		  | tailFold (a, b) = (tl a)::b

                (*
                headFold (a, b)
                TYPE: 'a list * 'a list -> 'a list
                PRE: true
                POST: b with the head of a as head. If a is the empty list then b.
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
getFromList word dictionary ml
TYPE: getFromList = int list -> string list list -> (char * int) list ref -> char list option * string list list
PRE: true
POST: A char list option of the first exploded string from dictionary that has the same structure as word (same character where it's the same int), NONE if no such string exists in dictionary. Also the tail of dictionary after a matching element has been found
EXAMPLE: getFromList [1,2,2] [["HI"],["FOO", "BAR"]] (ref [(#"F", 1), (#"O", 2), (#"B", 3)]) = (SOME [#"F", #"O", #"O"], [["BAR"]]):
VARIANT: length dictionary
*)
fun getFromList word nil _ = (NONE, [])
  | getFromList _ (nil::_) _ = (NONE, [])
  | getFromList word (dictionary as ((hd::tl)::t2)) ml =
    let 
	val testWord = explode hd
	val mlCopy = ref (!ml)
			 
        (*
        wordOK numberWord testWord ml
        TYPE: int list -> char list -> (char * int) list ref -> bool
        PRE: length numberWord = length testWord
        POST: true if all numberWord elements and testWord elements of the same position exist in the same element in ml or that they don't exist in ml at all
        EXAMPLE: wordOK [1, 2, 2] [#"F", #"O", #"O"] (ref [(#"F", 1), (#"O", 2), (#"R", 3)]) = true
        VARIANT: length numberWord and also length testWord
	*)
	fun wordOK nil nil ml = true
	  | wordOK (nrhd::nrtl) (whd::wtl) ml =
	    let
                (*
                isOK l n list
                TYPE: ''a -> ''b -> (''a * ''b) list -> bool
                PRE: true
                POST: true if l and n both exist in the same pair in the list or that none of them exist in the list, otherwise false
                EXAMPLE: isOK 1 #"A" [(1, #"A"), (2, #"B")] = true
                VARIANT: length list
                *)
		fun isOK _ _ nil = true
		  | isOK l n ((ll, ln)::tl) = 
		    if l = ll then 
			if n = ln then true
			else false
		    else if n = ln then 
			false
		    else isOK l n tl

                (*
                partOf (p, list)
                TYPE: (char * int) * (char * int) list -> bool
                PRE: true
                POST: true if p is equal to an element in list otherwise false
                EXAMPLE: partOf ((#"B", 2), [(#"A", 1), (#"B", 2), (#"C", 3)]) = true
                VARIANT: length list
                *)
		fun partOf (p : (char*int), [] : (char*int) list) = false
		  | partOf (p, hd::tl) =
		    if not ((#1 p) = (#1 hd)) andalso not (partOf(p, tl)) then
			false
		    else
			true
	    in 
		(isOK whd nrhd (!ml)) andalso ( if partOf((whd, nrhd), !ml) then 
						    () 
						else 
						    ml := ((whd, nrhd)::(!ml)) ;
						(wordOK nrtl wtl ml))
	    end
    in
	if (length testWord) = (length word) then
	    if  wordOK word testWord mlCopy then 
		(ml := (!mlCopy); (SOME testWord, [tl]))
	    else
		getFromList word [tl] ml
	else 
	    getFromList word t2 ml
    end


(*
solve puzzle
TYPE: cPuzzle -> (char * int) list option
PRE: none
POST: 
EXAMPLE:
*)
fun solve puzzle = 
    let
        (*
        solve' (puzzle, dict, ml)
        TYPE:
        PRE:
        POST:
        EXAMPLE:
        VARIANT: length dict
        *)
	fun solve' (_, [], _) = false
	  | solve' ([], _, _) = true
	  | solve' (puzzle as head::tl, dict, ml) = 
	    let
		val mlCopy = ref (!ml)
		val result = getFromList head dict ml
	    in		  
		if isSome (#1result) andalso solve' (tl, longlist, ml) then 
		    true
		else
		   (ml := (!mlCopy); solve' (puzzle, #2result, ml))	
	    end
	val ml = ref []
    in
	if solve'(preprocess puzzle, longlist, ml) then 
	    SOME (rev (!ml))
	else
	    NONE
    end

(* Tests *)
val testPuzzle1 = [[1,2,3,1,1],[4,0,1,0,3],[3,1,4,5,2],[2,0,5,0,1],[2,3,1,4,3]]
