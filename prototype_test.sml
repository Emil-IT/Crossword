

fun toHorizontal ([], acc) = rev acc
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
   | cwCompare (p1::puzzle, []) = cwCompare(puzzle, p1)
   | cwCompare (p1::puzzle, compareAcc) = 
     let
         fun letterCalc ([], a) = a
           | letterCalc ([]::puzzle, a) = letterCalc(puzzle, a)
           | letterCalc ((p::ps)::puzzle, a) = if p = 0 then 
                                                   letterCalc(ps::puzzle, a) 
                                               else 
                                                   letterCalc(ps::puzzle, a+1)
							     
         fun areaCalc (p::puzzle) = length p * ((length puzzle)+1)
     in
         if letterCalc(p1, 0) > letterCalc(compareAcc, 0) then
             cwCompare(puzzle, compareAcc)
         else if letterCalc(p1, 0) < letterCalc(compareAcc, 0) then
             cwCompare(puzzle, p1)
         else
             if areaCalc(p1) > areaCalc(compareAcc) then
                 cwCompare(puzzle, compareAcc)
             else
                 cwCompare(puzzle, p1)
     end


 fun main' ([], cw) = [cw]
   | main' (ilist::ilists, cw) = 
     let
	 val hpuzzle = toHorizontal(cw, [])
	 val allCombos = findPlacement'(ilist, cw, findPosition(ilist, cw, (0,0,0))) @ (foldr (fn(p,l) => (toHorizontal(p, []))::l) [] (findPlacement'(ilist, hpuzzle, findPosition(ilist, hpuzzle, (0,0,0)))))
     in
	 foldr (fn (x,xs) => main'(ilists, x)@xs) [] allCombos
     end
	 

(*
fun main ([], cwList) = cwCompare cwList
  | main (ilist::ilists, cwList) = 
    let
	
    in
    end
*)
