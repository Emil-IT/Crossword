(* saveCrossword (cw, file)
TYPE: cPuzzle*string -> unit
PRE: file is the name of a textfile where cw should be saved
POST: unit
SIDE-EFFECTS: cw, converdet to a tring is added to the textfile with the name file
*)
fun saveCrossword (cw, file) =
    let
	fun cwToString [] = "\n"
	  | cwToString ([]::cw) = "|"^(cwToString cw)
	  | cwToString ((h::t)::cw)  = (Int.toString h)^" "^(cwToString (t::cw))
	
	val ostrm = TextIO.openAppend file
    in
	TextIO.output (ostrm, cwToString cw);
	TextIO.closeOut ostrm
    end

(* loadCrossword (n, file)
TYPE: int*string -> cPuzzle option
PRE: n >= 1. file is the name of a textfile containing crosswords
POST: The n:th crossword stored in file. If no crossword was found, then NONE.
*)
fun loadCrossword (n, file) =
    let
	fun toCw ([#"\n"], _, [], acc3) = rev acc3
	  | toCw ([#"\n"], _, acc2, acc3) = rev( (rev acc2)::acc3)
	  | toCw ((#"|")::str, acc1, acc2 , acc3) = toCw(str, acc1,[], (rev acc2)::acc3)
	  | toCw (#" "::str, acc1, acc2, acc3) = toCw(str, "", valOf(Int.fromString acc1)::acc2 , acc3)
	  | toCw (c::str, acc1, acc2, acc3) = 
	    if Char.isDigit c then
		toCw(str, acc1^(Char.toString c), acc2, acc3)
	    else
		toCw(str, acc1, acc2, acc3)

	fun findLine (1, istrm) = (case TextIO.inputLine istrm of NONE => "" | SOME line => line)
          | findLine (n, istrm) = (case TextIO.inputLine istrm of NONE => "" | SOME _ => findLine (n-1, istrm))

	val istrm = TextIO.openIn file
    in
	case findLine (n, istrm) of 
	    "" => (TextIO.closeIn istrm ; NONE)
	  | line => (TextIO.closeIn istrm ; SOME (toCw(explode line, "", [], [])))
    end
