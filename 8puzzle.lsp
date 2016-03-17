( defun 8puzzle ( &optional ( puzzle nil ) )
    ;Load program files
    ( load 'search)
    ( load 'utilities )
	
	(cond
		((equal puzzle nil)
			(format t "Please enter the start position as 9 digits 0-9 separated by white space in row major order~%")
			
			(setf val (read-line)) ; get the user input
			; loop through the string and get a list of integers and put in puzzle
			(setf puzzle (loop for (integer index) := (multiple-value-list 
                   (parse-integer val
                    :start (or index 0)
                    :junk-allowed t))
				:while integer
				:collect integer))
		)
	
	)

    ;Debug
    ( printPuzzle puzzle )
    ( mapcar #'printPuzzle ( bfs puzzle ) )
)

;Script commands for program when run from command line
( load 'readPuzzle )
(cond
	((= (length *ARGS*) 1)
		( 8puzzle ( getPuzzleFile *ARGS* ) )
	)
	(t
		(8puzzle)
	)
)