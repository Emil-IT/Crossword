(* CryptoCrossword *)
use "dictionarys.sml";

datatype 'a cPuzzle = CP of 'a list list
val empty = CP([])


	


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
