#|
 | Function: generateSuccessors
 |
 | Description: This function generates the successors of
 | the current state of the puzzle.
 |
 | Parameters:
 |   L - current state for the puzzle
 |
 |#
(defun generateSuccessors (currState)    
    ( let 
        ;Local vars
        ( 
            ( pos ( position 0 currState ) )    ;Position of 0 in currState
            successors                          ;List of successors
        )
        
        ;Move up when not in top row
        ( when ( > pos 2 )
            ( let ( ( child ( copy-list currState ) ) )
                ( rotatef ( nth ( - pos 3 ) child ) ( nth pos child ) )	; move up
                ( push child successors )	; add new state to list
            )
        )
        
        ;Move left when not in leftmost column
        ( when ( /= 0 ( mod pos 3 ) )
            ( let ( ( child ( copy-list currState ) ) )
                ( rotatef ( nth ( 1- pos ) child ) ( nth pos child ) )	; move left
                ( push child successors )	; add new state to list
            )
        )
        
        ;Move right when not in rightmost column
        ( when ( /= 0 ( mod ( 1+ pos ) 3 ) )
            ( let ( ( child ( copy-list currState ) ) )
                ( rotatef ( nth ( 1+ pos ) child ) ( nth pos child ) )	; move right
                ( push child successors )	; add new state to list
            )
        )
        
        ;Move down when not in bottom row
        ( when ( < pos 6 )
            ( let ( ( child ( copy-list currState ) ) )
                ( rotatef ( nth ( + pos 3 ) child ) ( nth pos child ) )	; move down
                ( push child successors )	; add new state to list
            )
        )
        
        ( return-from generateSuccessors successors )
    )
)

#|
 | Function: goalState?
 |
 | Description: This funciton determines if the current puzzle
 | is the goal state. Below is the goal state for the 8 puzzle.
 |
 | 8-puzzle	
 | 1  2  3
 | 8  0  4
 | 7  6  5
 |
 | Parameters:
 |   currPuzzle - current state for the puzzle
 |
 |#
(defun goalState? ( currPuzzle )
    ;(let (g)
    ;    (setf g '( 1 2 3 8 0 4 7 6 5 ) )	; set the goal state
    ;    (if (equal L g) t nil)	; check if current state is the goal state
    ;)
    ( equal currPuzzle '( 1 2 3 8 0 4 7 6 5 ) )
)


#|
 | Function: printPuzzle
 |
 | Description: This function prints the current puzzle in a 3x3 grid
 |
 | Parameters:
 |   puzzle - Current state of puzzle to print
 |
 |#
( defun printPuzzle ( puzzle )
    ( format t "~d ~d ~d~%" ( nth 0 puzzle ) ( nth 1 puzzle ) ( nth 2 puzzle ) )
    ( format t "~d ~d ~d~%" ( nth 3 puzzle ) ( nth 4 puzzle ) ( nth 5 puzzle ) )
    ( format t "~d ~d ~d~%~%" ( nth 6 puzzle ) ( nth 7 puzzle ) ( nth 8 puzzle ) )
)


#|
 | Function: removeZeros
 |
 | Description: This function traverses the list holding all of the puzzle
 |   states in the solution path, and replaces the zero in each state with a 
 |   space in preparation for formatted output.
 |
 | Parameters:
 |   path - Solution path containing puzzle states
 |
 |#
( defun removeZeros ( path )
    ;Loop over each state in the solution path
    ( dolist ( currStep path NIL )
        ;Loop over each spot in the current step
        ( dotimes ( i 9 NIL )
            ;If current position is 0, replace with a space for output
            ( when ( = ( nth i  currStep ) 0 ) ( setf ( nth i  currStep ) " " ) )
        )        
    )
)


#|
 | Function: printSolutionPath
 |
 | Description: This function prints the solution path for the puzzle in the 
 |   specified format.
 |
 | Parameters:
 |   path - Solution path containing puzzle states
 |
 |#
( defun printSolutionPath( path )
    ( cond
        ;If no path, do nothing
        ( ( = ( length path ) 0 ) )
        
        ;If one step left in path
        ( ( = ( length path ) 1 )
            ;Get steps in path
            ( let ( ( step1 ( nth 0 path ) ) )
                ;Print remaining step
                ( format t "    ~d ~d ~d~%" 
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                )
                ( format t "    ~d ~d ~d~%"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                )
                ( format t "    ~d ~d ~d~%~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                )
            )
        )
        
        ;If two steps left in path
        ( ( = ( length path ) 2 )
            ( let ( ( step1 ( nth 0 path ) ) ( step2 ( nth 1 path ) ) )
                ;Print two remaining steps
                ( format t "    ~d ~d ~d          ~d ~d ~d~%"
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                    ( nth 0 step2 ) ( nth 1 step2 ) ( nth 2 step2 )
                )
                ( format t "    ~d ~d ~d    ->    ~d ~d ~d~%"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                    ( nth 3 step2 ) ( nth 4 step2 ) ( nth 5 step2 )                    
                )
                ( format t "    ~d ~d ~d          ~d ~d ~d~%~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                    ( nth 6 step2 ) ( nth 7 step2 ) ( nth 8 step2 )
                )
            )
        )
        
        ;If three steps left in path
        ( ( = ( length path ) 3 )
            ( let ( ( step1 ( nth 0 path ) )
                    ( step2 ( nth 1 path ) )
                    ( step3 ( nth 2 path ) ) )
                ;Print two remaining steps
                ( format t "    ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%"
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                    ( nth 0 step2 ) ( nth 1 step2 ) ( nth 2 step2 )
                    ( nth 0 step3 ) ( nth 1 step3 ) ( nth 2 step3 )
                )
                ( format t "    ~d ~d ~d    ->    ~d ~d ~d    ->    ~d ~d ~d~%"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                    ( nth 3 step2 ) ( nth 4 step2 ) ( nth 5 step2 )
                    ( nth 3 step3 ) ( nth 4 step3 ) ( nth 5 step3 )                    
                )
                ( format t "    ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                    ( nth 6 step2 ) ( nth 7 step2 ) ( nth 8 step2 )
                    ( nth 6 step3 ) ( nth 7 step3 ) ( nth 8 step3 )
                )
            )
        )
        
        ;If four or more steps left in path
        ( t
            ( let ( ( step1 ( nth 0 path ) )
                    ( step2 ( nth 1 path ) )
                    ( step3 ( nth 2 path ) )
                    ( step4 ( nth 3 path ) ) )
                ;Print two remaining steps
                ( format t "    ~d ~d ~d          ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%"
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                    ( nth 0 step2 ) ( nth 1 step2 ) ( nth 2 step2 )
                    ( nth 0 step3 ) ( nth 1 step3 ) ( nth 2 step3 )
                    ( nth 0 step4 ) ( nth 1 step4 ) ( nth 2 step4 )
                )
                ( format t "    ~d ~d ~d    ->    ~d ~d ~d    ->    ~d ~d ~d    ->    ~d ~d ~d"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                    ( nth 3 step2 ) ( nth 4 step2 ) ( nth 5 step2 )
                    ( nth 3 step3 ) ( nth 4 step3 ) ( nth 5 step3 ) 
                    ( nth 3 step4 ) ( nth 4 step4 ) ( nth 5 step4 )                    
                )
                ;Check if this is the last row of solution path
                ( if ( > ( length path ) 4 )
                    ;If another row will be printed
                    ( format t "    ->~%" )
                    ;If this row is the end of the solution path
                    ( format t "~%" )
                )
                ( format t "    ~d ~d ~d          ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                    ( nth 6 step2 ) ( nth 7 step2 ) ( nth 8 step2 )
                    ( nth 6 step3 ) ( nth 7 step3 ) ( nth 8 step3 )
                    ( nth 6 step4 ) ( nth 7 step4 ) ( nth 8 step4 )
                )
            )
            
            ;If more than four steps were left
            ( when ( > ( length path ) 4 )
                ;Recurse to print remaining steps
                ( printSolutionPath ( nthcdr 4 path ) )
            )
        )
    )
)


#|
 | Function: printBFSResults
 |
 | Description: This function prints the results of the BFS state space search.
 |
 | Parameters:
 |   path - Solution path containing puzzle states
 |
 |#
( defun printSearchResults ( path )
    ;Print results heading
    ( format t "Solution found in ~d moves~%" ( - ( length path ) 1 ) )
    ( format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%"
        *NUM_GEN*
        *NUM_DIST*
        *NUM_EXP*
    )
    
    ;Call helper function to replace 0's in solution with spaces for output
    ( removeZeros path )

    ;Print solution path
    ( printSolutionPath path )
)