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
                    Det viktigaste som saknas h�r �r att den ska automatiskt veta att det finns ledig plats n�r listorna tar slut, t.ex. att det st�r [0,1,0] och ens egna ord b�rjar p� ettan (med en etta) men �r typ 5 siffror l�ngt, 
                    vilket b�r ge tillbaka [0, 1, *, *, *, *] och �ven �ndra alla listors storlek genom att fylla i med nollor.
                    *)
                    fun findPlacement' (ilist', [], _) = NONE
                      | findPlacement' (ilist', puzzle', (x, y, z)) = 
                        let
                            val lengthlist = length ilist'
                            val listabove = List.drop(List.take(List.nth(puzzle', y-1), x+lengthlist-z), x-z)
                            val listy = List.drop(List.take(List.nth(puzzle', y), x+lengthlist-z), x-z)
                            val listbelow = List.drop(List.take(List.nth(puzzle', y+1), x+lengthlist-z), x-z)
                            fun notZero q = q <> 0
                        in
                            if length(map(notZero, listabove)) < 2 andalso length(map(notZero, listy)) < 2 andalso length(map(notZero, listbelow)) < 2 then 
                                SOME(List.take(puzzle', y-1) @ ilist' @ List.drop(puzzle', y))
                            else
                                NONE
                        end
                            
                    (*
                    Hittar en gemensam bokstav och returnerar koordinater d�r x �r positionen i en lista i puzzle, y �r positionen p� sj�lva listan i puzzle, och z �r positionen i int listan.
                    Denna beh�ver debuggas.
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
                                valOf(foo)
                            else
                                findPosition(i::is, ps::pss, (x+1, y, 0))
                        end
                in
                    findPlacement'(ilist, puzzle, findPosition(ilist, puzzle, (0,0,0)))
                end
        in
        end
in
end
