

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
	

 fun findPlacement' (ilist, [], []) = [[ilist], toHorizontal([ilist], [])]
   | findPlacement' (_, _, []) = []
   | findPlacement' (ilist', puzzle', (x, y, z)::posList) = 
     let
         val lengthlist = length ilist'
         val entirelisty = List.nth(puzzle', y)
                                   
         val listabove = if y >= 1 then
                             if x > z then 
                                 if (length(List.nth(puzzle', y-1)) - x) > (lengthlist - z) then
                                     List.drop(List.take(List.nth(puzzle', y-1), lengthlist), x-z)
                                 else 
                                     List.drop(List.nth(puzzle', y-1), x-z)
                             else
                                 if (length(List.nth(puzzle', y-1)) - x) > (lengthlist - z) then
                                     List.take(List.nth(puzzle', y-1), x+lengthlist-z)
                                 else
                                     List.nth(puzzle', y-1)
                         else
                             []
                                 
         val listy = if x > z then
                         if (length(entirelisty) - x) > (lengthlist - z) then
                             List.drop(List.take(entirelisty, lengthlist+2), x-z-1)
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
                                     List.drop(List.take(List.nth(puzzle', y+1), lengthlist), x-z)
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
           | addBefore (n, l::list) =  [zeroMaker(n, []) @ l] @ addBefore(n, list)
         fun addAfter (_, []) = []
           | addAfter (n, l::list) = [l @ zeroMaker(n, [])] @ addAfter(n, list)
     in
         if length(List.filter notZero listabove) < 2 andalso length(List.filter notZero listy) < 2 andalso length(List.filter notZero listbelow) < 2 then 
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


 fun main' ([], cw) = [cw]
   | main' (ilist::ilists, cw) = 
     let
	 val cwT = transpose(cw, [])
	 val allCombos = findPlacement'(ilist, cw, findPosition(ilist, cw, (0,0,0))) @ (foldr (fn(p,l) => (transpose(p, []))::l) [] (findPlacement'(ilist, cwT, findPosition(ilist, cwT, (0,0,0)))))
     in
	 foldr (fn (x,xs) => main'(ilists, x)@xs) [] allCombos
     end
	 
fun generate words =
    cwCompare(main'(
