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
)