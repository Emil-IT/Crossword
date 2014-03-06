


fun findPlacement (ilist, [], []) = [[ilist], transpose([ilist], [])]
  | findPlacement (_, _, []) = []
  | findPlacement (ilist', puzzle', (x, y, z)::posList) = 
    let
        val lengthlist = length ilist'
        val entirelisty = List.nth(puzzle', y)
                                  
        val listabove = if y >= 1 then
                            if x > z then 
                                if (length(List.nth(puzzle', y-1)) - x) > (lengthlist - z) then
                                    List.take(List.drop(List.nth(puzzle', y-1), x-z), lengthlist)
                                else 
                                    List.drop(List.nth(puzzle', y-1), x-z)
                            else
                                if (length(List.nth(puzzle', y-1)) - x) > (lengthlist - z) then
                                    List.take(List.nth(puzzle', y-1), x + lengthlist-z)
                                else
                                    List.nth(puzzle', y-1)
                        else
                            []
                                
        val listy = if x > z then
                        if (length(entirelisty) - x) > (lengthlist - z) then
                            List.take(List.drop(entirelisty, x-z-1),lengthlist+2)
                        else
                            List.drop(entirelisty, x-z-1)
                    else
                        if (length(entirelisty) - x) > (lengthlist - z) then
                            List.take(entirelisty, x+lengthlist-z + 1)
                        else
                            entirelisty
                                
        val listbelow = if (length puzzle' - y) > 1 then
                            if x > z then 
                                if (length(List.nth(puzzle', y+1)) - x) > (lengthlist - z) then
                                    List.take(List.drop(List.nth(puzzle', y+1), x-z), lengthlist)
                                else
                                    List.drop(List.nth(puzzle', y+1), x-z)
                            else
                                if (length(List.nth(puzzle', y+1)) - x) > (lengthlist - z) then
                                    List.take(List.nth(puzzle', y+1), x+lengthlist-z)
                                else
                                    List.nth(puzzle', y+1)
                        else
                            []
                                
        fun notZero q = q <> 0
        fun zeroMaker (0, acczero) = acczero
          | zeroMaker (n, acczero) = zeroMaker(n-1, [0] @ acczero)
        fun addBefore (_, []) = []
          | addBefore (n, l::list) = [zeroMaker(n, []) @ l] @ addBefore(n, list)
        fun addAfter (_, []) = []
          | addAfter (n, l::list) = [l @ zeroMaker(n, [])] @ addAfter(n, list)
        fun checkList () = 
            let
                fun checkList' ([], [], [], []) = true
                  | checkList' (l1::list1, [], [], l4::list4) = if  l1 = 0 orelse l1 = l4 then
                                                                    checkList'(list1, [], [], list4)
                                                                else
                                                                    false
                  | checkList' (l1::list1, [], l3::list3, l4::list4) = if (l1 = 0 andalso l3 = 0) orelse l1 = l4 then
                                                                           checkList'(list1, [], list3, list4)
                                                                       else
                                                                           false
                  | checkList' (l1::list1, l2::list2, [], l4::list4) = if (l1 = 0 andalso l2 = 0) orelse l1 = l4 then
                                                                           checkList'(list1, list2, [], list4)
                                                                       else
                                                                           false
                  | checkList'(l1::list1, l2::list2, l3::list3, l4::list4) = if (l1 = 0 andalso l2 = 0 andalso l3 = 0) orelse l1 = l4 then 
                                                                                 checkList'(list1, list2, list3, list4)
                                                                             else
                                                                                 false
            in
                if x > z then
                    if length(entirelisty) - x > lengthlist - z then
                        if hd(listy) = 0 andalso List.nth(listy, length(listy)-1) = 0 then
                            checkList'(List.take(tl(listy), length(listy) - 2), listabove, listbelow, ilist')
                        else
                            false
                    else
                        if hd(listy) = 0 then
                            checkList'(tl(listy), listabove, listbelow, List.take(ilist', length(listy)-1))
                        else
                            false
                else
                    if length(entirelisty) - x > lengthlist - z then
                        if List.nth(listy, length(listy)-1) = 0 then
                            checkList'(List.take(listy, length(listy) - 1), listabove, listbelow, List.drop(ilist', lengthlist - length(listy) + 1))
                        else
                            false
                    else
                        checkList'(listy, listabove, listbelow, List.take(List.drop(ilist', z-x), length(listy)))
            end
    in
        
        if checkList () then
            if x > z then
                if length(entirelisty) - x > lengthlist - z then
                    (List.take(puzzle', y) @ 
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     List.drop(puzzle', y+1)) :: findPlacement(ilist', puzzle', posList)
                else
                    (addAfter(lengthlist-z - length(entirelisty)+x, List.take(puzzle', y)) @
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     addAfter(lengthlist-z - length(entirelisty)+x, List.drop(puzzle', y+1))) :: findPlacement(ilist', puzzle', posList)
            else
                if length(entirelisty) - x > lengthlist - z then
                    (addBefore(z-x, List.take(puzzle', y)) @
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     addBefore(z-x, List.drop(puzzle', y+1))) :: findPlacement(ilist', puzzle', posList)
                else
                    (addAfter(lengthlist-z-length(entirelisty)+x, addBefore(z-x, List.take(puzzle', y))) @
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     addAfter(lengthlist-z-length(entirelisty)+x, addBefore(z-x, List.drop(puzzle', y+1)))) :: findPlacement(ilist', puzzle', posList)
        else
            findPlacement(ilist', puzzle', posList)
    end
	

fun findPosition (_, [], _) = []
  | findPosition (i, []::pss, (x, y, z)) = findPosition (i, pss, (0, y+1, 0))
  | findPosition (i::is, pz as (p::ps)::pss, (x, y, z)) = 
    let 
        fun findPosition' ([], pz, (x', y', z')) = []
          | findPosition' (i'::is', pz' as (p'::ps')::pss', (x', y', z')) = if i' = p' then 
                                                                                (x', y', z')::findPosition' (is', pz', (x', y', z'+1))
                                                                            else
                                                                                findPosition' (is', pz', (x', y', z'+1))
        val resultPos = findPosition' (i::is, pz, (x, y, z))
    in
        resultPos @ findPosition(i::is, ps::pss, (x+1,y,0))
    end
	
fun cwCompare ([], compareAcc) = compareAcc
  | cwCompare (cw::cwList, []) = cwCompare(cwList, cw)
  | cwCompare (cw::cwList, compareAcc) = 
    let
	fun letterCalc ([], a) = a
	  | letterCalc ([]::cw, a) = letterCalc(cw, a)
	  | letterCalc ((p::ps)::cw, a) = 
	    if p = 0 then 
		letterCalc(ps::cw, a) 
	    else 
		letterCalc(ps::cw, a+1)
			  
	fun areaCalc (row1::cw) = length row1 * ((length cw)+1)
						    
	val cwLetters = letterCalc(cw, 0)
	val accLetters = letterCalc(compareAcc, 0)
    in
	if cwLetters > accLetters then
	    cwCompare(cwList, compareAcc)
	else if cwLetters < accLetters then
	    cwCompare(cwList, cw)
	else
	    if areaCalc(cw) > areaCalc(compareAcc) then
		cwCompare(cwList, compareAcc)
	    else
		cwCompare(cwList, cw)
    end	
	

fun stringToInt s =
    let 
	fun stringToInt' ([], _, _, acc) = rev acc
	  | stringToInt' (s::sl, ml, n, acc) = 
	    let

		(* clToInt (cl, ml, n, acc)
		TYPE:  char list * (char * int) list * int * int list -> (char * int) list * int * int list
		PRE: n is greater then the highest number allready associated with a character in ml.
		POST: ml with the new char-int matches inserted. n incremented as many times as the number of chars inserted to ml. cl with the characters translated to numbers with the help of ml.
                VARIANT: length cl
		EXAMPLE: 
		 *)
		fun clToInt ([], ml : (char*int) list, n, acc) = (ml, n, rev acc)
		  | clToInt (c::cl, [], n, acc) = clToInt(cl, [(c,n)], n+1, n::acc)
		  | clToInt (c::cl, ml, n, acc) = 
		    let 

			(* getNr (c1, ml as (c2, n)::tl)
			TYPE: ''a * (''a * 'b) list -> 'b option
			PRE: true
			POST: The value paired with a value equal to c1 in ml as an option type. If no such value exists, then NONE.
			VARIANT: length ml
			EXAMPLE: gerNr (#"F", [(#"B", 4), (#"A", 2), (#"R", 1), (#"F", 3), (#"O", 5)]) = SOME 3
			 *)
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
	
	
 
(* generate words
TYPE: string list -> int list list
PRE: true
POST: the most compact crypto crossword that can be built with the words in words
EXAMPLE: generate ["STASS", "SKATT", "ASKES", "SATSA","ASKET", "TASKA"] =  [[1,2,3,1,1],
                                                                            [4,0,1,0,3],
                                                                            [3,1,4,5,2],
                                                                            [2,0,5,0,1],
                                                                            [2,3,1,4,3]]
*)	 
fun generate words =
    let	

	(* generate' (ilists, cw, n)
	TYPE: int list list * cPuzzle * int -> int list list list
	PRE: true
	POST: A list of crosswords with different ways to insert the words in ilists into cw
	VARIANT: length ilists
	EXAMPLE:  generate'([[1,2,3],[2,3,4]], [], 0) = [[[1,2,3],[0,3,0],[0,4,0]],[[0,0,2],[1,2,3],[0,0,4]],
                                                         [[1,0,0],[2,3,4],[3,0,0]],[[0,1,0],[0,2,0],[2,3,4]],
                                                         [[1,0,0],[2,3,4],[3,0,0]],[[0,1,0],[0,2,0],[2,3,4]],
                                                         [[1,2,3],[0,3,0],[0,4,0]],[[0,0,2],[1,2,3],[0,0,4]]]
	 *)			
	fun generate' ([], cw, _) = [cw]
	  | generate' (ilist::ilists, cw, n) = 
	    let

		(* transpose (cw, acc)
		TYPE: ''a list list * ''a list list -> ''a list list
                PRE: All lists in cw are the same length
                POST: cw transposed and concatinated with acc.
                VARIANT: length of the lists in cw
                EXAMPLE: transpose([[1,2,3],[4,5,6],[7,8,9]], []) = [[1,4,7],[2,5,8],[3,6,9]]
		 *)
		fun transpose ([], acc) = rev acc
		  | transpose (cw, acc) = 
		    let
			(* tailFold (a, b)
                        TYPE: 'a list * 'a list list -> 'a list list
                        PRE: true
                        POST: b with the tail of a as head. If a is the empty list then b.
                        EXAMPLE: tailFold ([1,2,3], [[0], [1,2]]) = [[2, 3], [0], [1, 2]]
			*)
			fun tailFold ([], b) = b
			  | tailFold (a, b) = (tl a)::b
							  
			(* headFold (a, b)
                        TYPE: 'a list * 'a list -> 'a list
                        PRE: true
                        POST: b with the head of a as head. If a is the empty list then b.
                        EXAMPLE: headFold ([1,2,3], [4,5,6]) = [1, 4, 5, 6]: int list
			*)
			fun headFold ([], b) = b
			  | headFold (a, b) = (hd a)::b
			val nextList = foldr headFold [] cw
		    in
			transpose (foldr tailFold [] cw, if nextList = [] then acc else nextList::acc)
		    end
			
		(* zeros n
		TYPE: int -> int list
		PRE: n >= 0
		POST: A list of n zeros
		VARIANT: n
		EXAMPLE: zeros 5 = [0,0,0,0,0]
		 *)	
		fun zeros 0 = []
		  | zeros n = 0::zeros(n-1)
		val cwT = transpose(cw, [])
		val allCombos = findPlacement'(ilist, cw, findPosition(ilist, cw, (0,0,0))) @ (foldr (fn(p,l) => (transpose(p, []))::l) [] (findPlacement'(ilist, cwT, findPosition(ilist, cwT, (0,0,0)))))
												  
	    in
		case allCombos of [] => if n <= length(ilist::ilists) then
					    generate'(ilists@[ilist], cw, n+1) 
					else 
					    generate'(ilists ,hd (findPlacement'(ilist, (zeros(length (hd cw)))::(zeros(length (hd cw)))::cw, [(0,0,0)])), 0)
				| _ => foldr (fn (x,xs) => generate'(ilists, x, 0)@xs) [] allCombos
	    end


		
    in	
	cwCompare(generate'(stringToInt words, [], 0),[])
    end
