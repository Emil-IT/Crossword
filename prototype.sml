(*
Tanken med algoritmen f�r hela generatorn �r att den ska antingen (min f�rsta id�) hela tiden "fr�ga" n�sta varv om den godk�nner positionen/ordet/bokstaven genom att skicka vidare den i en if-sats, d�r slutvarvet kommer returnera true om korsordet blev komplett (att alla ord sitter ihop p� n�got s�tt) och false om det inte blev. Returnerar den false kommer f�rst den "n�rmaste" funktionen att testa vidare, om den inte hittar n�got kommer den funktionen innan det att testa vidare ett steg f�r att sedan g� igenom den "n�rmaste" funktionen igen om och om igen �nda tills precis ALLA kombinationer har testats.

Den andra id�n (som �r mycket b�ttre imo) �r att vi har en accumulator f�r generatorn som sparar undan det "b�sta" pusslet. Detta kommer fungera genom att den g�r samma sak som den andra �nda tills den f�r sitt f�rsta resultat, men ist�llet f�r att skicka tillbaka true s� l�ggs pusslet in i accumulatorn. Efter detta testar den "n�rmsta" funktionen n�sta resultat och j�mnf�r det nya resultatet med accumulatorn. Det "b�sta" resultatet sparas i accumulatorn och sedan forts�tter loopen �nda tills precis ALLA kombinationer har testats och det absolut b�sta pusslet �r kvar. J�mnf�relsen av resultaten kan prioriteras i ordningen av vilken som har flest ord ihopsatta f�rst och efter det arean p� sj�lva pusslet. 

Jag har funderat i flera timmar p� hur algoritmen skulle se ut och det �r detta jag har kommit fram till, ni f�r g�rna fr�ga mig om det �r n�got ni undrar och ni f�r �ven g�rna l�gga till n�got om ni har b�ttre id�er.

*)

fun generator l = 
let
    fun generatorAux (l, acc) =
        let
            fun findPlacement (ilist, puzzle) = 
                let
                    (*
                    L�ngt ifr�n klar, det �r t�nkt att man laddar in en int list, puzzle och koordinater f�r en gemensam bokstav, denna kollar listorna ovanf�r, under, och listan d�r ordet ska s�ttas in. 
                    Tanken var att man kallar p� denna funktion f�rst med puzzle utskriven i v�gr�ta listor, sedan lodr�ta om det ej fanns resultat.
                    *)
                    fun findPlacement' (ilist', [], _) = NONE
                      | findPlacement' (ilist', puzzle', (x, y, z)) = 
                        let
                            val lengthlist = length ilist'
                            val entirelisty = List.nth(puzzle', y)
                                                      
                            val listabove = if y >= 1 then
                                                if x >= z then 
                                                    if (length(List.nth(puzzle', y-1)) - x) > (lengthlist - z) then
                                                        List.drop(List.take(List.nth(puzzle', y-1), lengthlist+1), x-z)
                                                    else 
                                                        List.drop(List.nth(puzzle', y-1), x-z)
                                                else
                                                    if (length(List.nth(puzzle', y-1)) - x) > (lengthlist - z) then
                                                        List.take(List.nth(puzzle', y-1), x+lengthlist-z)
                                                    else
                                                        List.nth(puzzle', y-1)
                                            else
                                                []
                                                    
                            val listy = if x >= z then
                                            if (length(entirelisty) - x) > (lengthlist - z) then
                                                List.drop(List.take(entirelisty, lengthlist+1), x-z)
                                            else
                                                List.drop(entirelisty, x-z)
                                        else
                                            if (length(entirelisty) - x) > (lengthlist - z) then
                                                List.take(entirelisty, x+lengthlist-z)
                                            else
                                                entirelisty
                                                    
                            val listbelow = if length puzzle' > 1 then
                                                if x >= z then 
                                                    if (length(List.nth(puzzle', y+1)) - x) >(lengthlist - z) then
                                                        List.drop(List.take(List.nth(puzzle', y+1), lengthlist+1), x-z)
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
                                        SOME(List.take(puzzle', y) @ 
                                        [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                                        List.drop(puzzle', y+1))
                                    else
                                        SOME(addAfter(lengthlist-z - length(entirelisty)+x, List.take(puzzle', y)) @
                                        [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                                        addAfter(lengthlist-z - length(entirelisty)+x, List.drop(puzzle', y+1)))
                                else
                                    if length(entirelisty) - x > lengthlist - z then
                                        SOME(addBefore(z-x, List.take(puzzle', y)) @
                                        [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                                        addBefore(z-x, List.drop(puzzle', y+1)))
                                    else
                                        SOME(addAfter(lengthlist-z-length(entirelisty)+x, addBefore(z-x, List.take(puzzle', y))) @
                                        [(if x > z then List.take(entirelisty, x-z) else []) @ ilist' @ (if length(entirelisty) - x > lengthlist-z then List.drop(entirelisty, x+lengthlist-z) else [])] @ 
                                        addAfter(lengthlist-z-length(entirelisty)+x, addBefore(z-x, List.take(puzzle', y))))
                            else
                                NONE
                            
                        end



                            
                    (*
                    Hittar en gemensam bokstav och returnerar koordinater d�r x �r positionen i en lista i puzzle, y �r positionen p� sj�lva listan i puzzle, och z �r positionen i int listan.
                    *)
                    fun findPosition (_, [], _) = NONE
                      | findPosition (i, []::pss, (x, y, z)) = findPosition (i, pss, (0, y+1, 0))
                      | findPosition (i::is, pz as (p::ps)::pss, (x, y, z)) = 
                        let 
                            fun findPosition' ([], pz, (x', y', z')) = NONE
                              | findPosition' (i'::is', pz' as (p'::ps')::pss', (x', y', z')) = if i' = p' then 
                                                                                                     SOME (x', y', z')
                                                                                                else
                                                                                                    findPosition' (is', pz', (x', y', z'+1))
                            val foo = findPosition' (i::is, pz, (x, y, z))
                        in
                            if isSome foo then 
                                foo
                            else
                                findPosition(i::is, ps::pss, (x+1, y, 0))
                        end

                    val result = findPlacement'(ilist, puzzle, valOf(findPosition(ilist, puzzle, (0,0,0))))
                in
                    result
                (*
                SPEKULATION:
                if isSome result then result else toHorizontal(findPlacement(ilist, getVer(puzzle, length puzzle), (0,0,0))) 
                *)
                end
        in
        end
in
end
