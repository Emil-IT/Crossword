use "CryptoCrossword.sml"
use "generator.sml"

fun tests n = 
let
	fun testcase 0 = 
		solve [] = SOME [] andalso
		generate [] = []

	| testcase 1 =
	solve [[1,2,3,1,1],[4,0,1,0,3],[3,1,4,5,2],[2,0,5,0,1],[2,3,1,4,3]] 
			= SOME [(#"S", 1), (#"T", 2), (#"A", 3), (#"K", 4), (#"E", 5)]
	andalso
	solve [[1,2,1,1], [2,0,0,2], [3,2,1,1]] 
			= SOME [(#"T", 1), (#"U", 2), (#"R", 3)]
  
  
	| testcase 2 = 
	generate ["HUBBA", "BUBBA"] 
				= [[0, 0, 0, 0, 3], [0, 0, 0, 0, 2], [0, 0, 0, 0, 3], [0, 0, 0, 0, 3],[1, 2, 3, 3, 4]]
	andalso
	generate ["VISKA", "VELAR", "SAKTA", "LEKAR"] 
				= [[1, 2, 3, 4, 5], [6, 0, 5, 0, 0], [7, 6, 4, 5, 8], [5, 0, 9, 0, 0], [8, 0, 5, 0, 0]]
 
 

	in
		(("Empty Inputs to generate and solve", testcase 0),
		("Standard Inputs to solve", testcase 1),
		("Standard Inputs to generate", testcase 2))
	end;
	
	val result = tests 0;	
	