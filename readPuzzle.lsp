 #|
 | Function: getPuzzleFile
 |
 | Description:
 | Gets the puzzle file from the command line
 |
 | Parameters:
 | args - command line arguments
 |
 |#
 ( defun getPuzzleFile ( args )
    ( cond 
        ;If exactly one command line argument given
        ( ( = 1 ( length args ) )
            ;Get puzzle from specified file
            ( readPuzzleFile ( car args ) )
        )
        
        ;If not exactly one command line argument given
        ( t
            ;Required command line argument missing, print usage statement
            (format t "Usage: clisp 8puzzle.lsp puzzlefile~%")
        )
    )
)

#|
 | Function: readPuzzleFile
 |
 | Description:
 | Read in a puzzle from a file given at the command line.
 |
 | Parameters:
 | filename - name of the puzzle file
 |
 |#
(defun readPuzzleFile (filename)
	(with-open-file (in filename)
		(loop for number = (read in nil)
			until (null number)
			collect number
		)			
	)
)
#|
 | Function: parseList
 |
 | Description:
 | This function checks to make sure that the user inputed
 | a usable puzzle.
 |
 | Parameters:
 | puzList - Starting state
 |
 |#
(defun parseList (puzList)
	(loop for i in puzList do
		(if (not (numberp i))		
			(format t "error puzzle list contains a non-number")
			(readStart args)
		)
	)
)