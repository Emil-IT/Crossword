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

fun loadCrossword (n, file) =
    let
	fun toCw ([#"\n"], acc1, acc2, acc3) = acc2::acc3
	  | toCw ((#"|")::str, acc1, acc2 , acc3) = toCw(str, [], acc21::acc3)
	  | toCw (c::str, acc1, acc2, acc3) = 
