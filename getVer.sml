
fun getVer' ([], n) = []
  | getVer' (l, n) = List.drop((List.take((hd(l)), n)), n-1) @ getVer' (tl(l), n)
 
  fun getVer (l, 0) = []
    | getVer (l, n)  = getVer' (l, n) :: getVer (l, n-1)