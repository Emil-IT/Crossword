(* CryptoCrossword *)
use "dictionarys.sml";

(*
REPRESENTATION CONVENTION:
REPRESENTATION INVARIANT:
*)
datatype 'a cPuzzle = CP of 'a list list
val empty = CP([])

fun toHorizontal ([], acc) = acc
  | toHorizontal (puzzle, acc) = 
    let
	fun tailFold ([], b) = b
	  | tailFold (a, b) = (tl a)::b
	fun headFold ([], b) = b
	  | headFold (a, b) = (hd a)::b
	val nextList = foldr headFold [] puzzle
    in
	toHorizontal (foldr tailFold [] puzzle, if nextList = [] then acc else nextList::acc)
    end
 
(*
preprocess puzzle
TYPE: int list list -> int list list
PRE: true
POST: 
EXAMPLE: preprocess [[1,2,3],[0,1,2],[],[0,1,0]] = [[1, 2, 3], [1, 2]]
VARIANT: length puzzle
*)
fun preprocess [] = []
  | preprocess (p::puzzle) =
    let
        (*
        preprocess' (list, acc)
        TYPE: int list * int list -> int list list
        PRE: true
        POST: int list list with the elements being the groups of non-zero numbers in a row (and excluding groups of length 1)
        EXAMPLE: preprocess' ([1,2,0,3,4,0,1], []) = [[1, 2], [3, 4]]
        VARIANT: length list
        *)
        fun preprocess' ([], []) = []
	  | preprocess' ([], [a]) = []
	  | preprocess' ([], acc) = (rev acc)::[]
	  | preprocess' (0::list, [a]) = preprocess' (list, [])
          | preprocess' (0::list, acc) = (rev acc)::(preprocess' (list, [])) 
          | preprocess' (l::list, acc) = preprocess'(list, l::acc)
        val a = preprocess' (p, [])
    in
        a @ preprocess puzzle
    end


(*
getFromList 
TYPE:
PRE:
POST:
EXAMPLE:
VARIANT:
*)
fun getFromList word nil _ = NONE
  | getFromList _ (nil::_) _ = NONE
  | getFromList word ((hd::tl)::t2) ml =
    let 
	val testWord = explode hd
	val ordlista = ref (!ml)
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
	    if  wordOK word testWord ordlista then 
		(ml := (!ordlista); SOME testWord)
	    else
		(ordlista := (!ml); getFromList word (tl::t2) ordlista)
	else 
	    getFromList word t2 ml
    end
