
fun transpose ([], acc) = rev acc
  | transpose (cw, acc) = 
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
	val nextList = foldr headFold [] cw
    in
	transpose (foldr tailFold [] cw, if nextList = [] then acc else nextList::acc)
    end
	

fun findPlacement' (ilist, [], []) = [[ilist], transpose([ilist], [])]
  | findPlacement' (_, _, []) = []
  | findPlacement' (ilist', puzzle', (x, y, z)::posList) = 
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
                     List.drop(puzzle', y+1)) :: findPlacement'(ilist', puzzle', posList)
                else
                    (addAfter(lengthlist-z - length(entirelisty)+x, List.take(puzzle', y)) @
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     addAfter(lengthlist-z - length(entirelisty)+x, List.drop(puzzle', y+1))) :: findPlacement'(ilist', puzzle', posList)
            else
                if length(entirelisty) - x > lengthlist - z then
                    (addBefore(z-x, List.take(puzzle', y)) @
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     addBefore(z-x, List.drop(puzzle', y+1))) :: findPlacement'(ilist', puzzle', posList)
                else
                    (addAfter(lengthlist-z-length(entirelisty)+x, addBefore(z-x, List.take(puzzle', y))) @
                     [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                     addAfter(lengthlist-z-length(entirelisty)+x, addBefore(z-x, List.drop(puzzle', y+1)))) :: findPlacement'(ilist', puzzle', posList)
        else
            findPlacement'(ilist', puzzle', posList)
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
			  

 fun main' ([], cw) = [cw]
   | main' (ilist::ilists, cw) = 
     let
	 val cwT = transpose(cw, [])
	 val allCombos = findPlacement'(ilist, cw, findPosition(ilist, cw, (0,0,0))) @ (foldr (fn(p,l) => (transpose(p, []))::l) [] (findPlacement'(ilist, cwT, findPosition(ilist, cwT, (0,0,0)))))
     in
	 foldr (fn (x,xs) => main'(ilists, x)@xs) [] allCombos
     end
	 
fun generate words =
    cwCompare(main'(stringToInt words, []),[])
