(defun read_start(args)
	(let (filename puzList)

		(cond
				((= (length args) 3)
					
					(setf filename (caddr args))
					(setf puzList (readPuzzleFile filename))
				)
				
				((= (length args) 2)
					
					(setf puzList (cadr args))
		
		
				)
				
				((= (length args) 1)
					
					(fromat t "Enter a list of numbers 0-8 in row major order. 0 is the blank for the puzzle")
					(setf puzList (read))
					(if ( not ( = (length puzList) 9))
					
						(fromat t "Error not enough numbers")
						(readStart args)
					
					)
					(parseList puzList)
				
				)
				
				; else statement prints usage statement
				(t
					; print out a usage statement for commandline:
					(format t "Usage Commandline: clisp 8puzzle.lsp puzzlefile~%")
					(format t "       clisp - runs clisp interpetter~%")
					(format t "       8puzzle.lsp - program file~%")
					(format t "       puzzlefile - file containing the postions of the numbers in the puzzle~%")
					
					; print out a usage statement for clisp:
					(format t "Usage clisp: 8puzzle puzzleList~%")
					(format t "       8puzzle - program call~%")
					(format t "       puzzlefile -  contains an 8-puzzle start position, stored in row-major order as
a list of 9 elements. The digits 1-8 represent the 8 tiles, and 0 represents the blank.~%")

					(format t "Usage clisp: 8puzzle~%")
					(format t "       8puzzle.lsp - program file~%")
				)
		)
	
	
	) 


)


(defun readPuzzleFile (filename)
	(with-open-file (in filename)
		(loop for number = (read in nil)
			until (null number)
			collect number
		)
			
	)
	

)

(defun parseList (puzList)

	(loop for i in puzList do

		(if (not (numberp i))
		
			(format t "error puzzle list contains a non-number")
			(readStart args)
		)



	)

}