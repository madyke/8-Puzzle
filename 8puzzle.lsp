#|
 | Program: 8-Puzzle
 | Authors: Matt Dyke, Marcus Berger, Cassidy Vollmer
 | Class: CSC 447 - Artificial Intelligence
 | Instructor: Dr. Weiss
 | Due Date: March 27, 2016
 |
 | Description:
 |
 | Input:
 | Output:
 | Compilation instructions: Run in CLisp on Linux or Windows
 | Usage:
 |#

;;------------------------------ Global Variables ------------------------------;

( defvar *NUM_GEN* )      ;Number of total nodes that were generated
( defvar *NUM_DIST* )     ;Number of distinct nodes that were generated
( defvar *NUM_EXP* )      ;Number of total nodes that were expanded


;--------------------------------- Functions ----------------------------------;

#|
 | Function: 8puzzle
 |
 | Description:
 |
 | Parameters:
 |   &optional ( puzzle nil ) - Starting state for puzzle
 |
 |#
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

    ;Reset global variables that track statistics of search    
    ( setf *NUM_GEN*  0 )
    ( setf *NUM_DIST* 0 )
    ( setf *NUM_EXP*  0 )
    
    ;Perform BFS and print results
    ( format t "~%BFS Graph Search~%" )
    ( format t "----------------~%" )
    ( printSearchResults ( BFS puzzle ) )
    
    ;Reset global variables that track statistics of search    
    ( setf *NUM_GEN*  0 )
    ( setf *NUM_DIST* 0 )
    ( setf *NUM_EXP*  0 )
    
    ;Perform DFID and print results
    ( format t "DFID Graph Search~%" )
    ( format t "-----------------~%" )
    ( printSearchResults ( DFID puzzle ) )
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