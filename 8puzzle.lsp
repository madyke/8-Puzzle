

;------------------------------ Global Variables ------------------------------;

( defvar *NUM_GEN* )      ;Number of total nodes that were generated
( defvar *NUM_DIST* )     ;Number of distinct nodes that were generated
( defvar *NUM_EXP* )      ;Number of total nodes that were expanded


( defun 8puzzle ( &optional ( puzzle nil ) )
    ;Load program files
    ( load 'search)
    ( load 'utilities )
    
    (setf *NUM_GEN* 0)
    (setf *NUM_DIST* 0 )
    (setf *NUM_EXP* 0 )
	
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
    ;( printPuzzle puzzle )
    ( printBFSResults ( bfs puzzle ) )
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