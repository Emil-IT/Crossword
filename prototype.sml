(*
Tanken med algoritmen för hela generatorn är att den ska antingen (min första idé) hela tiden "fråga" nästa varv om den godkänner positionen/ordet/bokstaven genom att skicka vidare den i en if-sats, där slutvarvet kommer returnera true om korsordet blev komplett (att alla ord sitter ihop på något sätt) och false om det inte blev. Returnerar den false kommer först den "närmaste" funktionen att testa vidare, om den inte hittar något kommer den funktionen innan det att testa vidare ett steg för att sedan gå igenom den "närmaste" funktionen igen om och om igen ända tills precis ALLA kombinationer har testats.

Den andra idén (som är mycket bättre imo) är att vi har en accumulator för generatorn som sparar undan det "bästa" pusslet. Detta kommer fungera genom att den gör samma sak som den andra ända tills den får sitt första resultat, men istället för att skicka tillbaka true så läggs pusslet in i accumulatorn. Efter detta testar den "närmsta" funktionen nästa resultat och jämnför det nya resultatet med accumulatorn. Det "bästa" resultatet sparas i accumulatorn och sedan fortsätter loopen ända tills precis ALLA kombinationer har testats och det absolut bästa pusslet är kvar. Jämnförelsen av resultaten kan prioriteras i ordningen av vilken som har flest ord ihopsatta först och efter det arean på själva pusslet. 

Jag har funderat i flera timmar på hur algoritmen skulle se ut och det är detta jag har kommit fram till, ni får gärna fråga mig om det är något ni undrar och ni får även gärna lägga till något om ni har bättre idéer.

*)

fun generator l = 
let
    fun generatorAux (l, acc) =
        let
            fun findPlacement (ilist, puzzle) = 
                let
                    (*
                    Långt ifrån klar, det är tänkt att man laddar in en int list, puzzle och koordinater för en gemensam bokstav, denna kollar listorna ovanför, under, och listan där ordet ska sättas in. 
                    Tanken var att man kallar på denna funktion först med puzzle utskriven i vågräta listor, sedan lodräta om det ej fanns resultat.
                    Det viktigaste som saknas här är att den ska automatiskt veta att det finns ledig plats när listorna tar slut, t.ex. att det står [0,1,0] och ens egna ord börjar på ettan (med en etta) men är typ 5 siffror långt, 
                    vilket bör ge tillbaka [0, 1, *, *, *, *] och även ändra alla listors storlek genom att fylla i med nollor.
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
                    Hittar en gemensam bokstav och returnerar koordinater där x är positionen i en lista i puzzle, y är positionen på själva listan i puzzle, och z är positionen i int listan.
                    Denna behöver debuggas.
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
