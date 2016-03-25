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
(defun generateSuccessors (L)
    (let (p q r s openList)
        (cond
            ((= (nth 0 L) 0)	; if the blank is in the top left
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 1 p) (nth 0 p) )	; move right
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 3 q) (nth 0 q) )	; move down
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
            )
            ((= (nth 1 L) 0) 
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 0 p) (nth 1 p) )	; move right
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 2 q) (nth 1 q) )	; move left
                    (setf r (copy-list L) )	; make copy of list
                    (rotatef (nth 4 r) (nth 1 r) )	; move down
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
                    (push r openList)	; add new state to list
            )
            ((= (nth 2 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 1 p) (nth 2 p) )	; move left
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 5 q) (nth 2 q) )	; move down
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
            )
            ((= (nth 3 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 0 p) (nth 3 p) )	; move up
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 4 q) (nth 3 q) )	; move right
                    (setf r (copy-list L) )	; make copy of list
                    (rotatef (nth 6 r) (nth 3 r) )	; move down
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
                    (push r openList)	; add new state to list
            )
            ((= (nth 4 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 1 p) (nth 4 p) )	; move up
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 3 q) (nth 4 q) )	; move left
                    (setf r (copy-list L) )	; make copy of list
                    (rotatef (nth 5 r) (nth 4 r) )	; move right
                    (setf s (copy-list L) )	; make copy of list
                    (rotatef (nth 7 s) (nth 4 s) )	; move down
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
                    (push r openList)	; add new state to list
                    (push s openList)	; add new state to list
            )
            ((= (nth 5 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 2 p) (nth 5 p) )	; move up
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 4 q) (nth 5 q) )	; move left
                    (append openList p)
                    (setf r (copy-list L) )	; make copy of list
                    (rotatef (nth 8 r) (nth 5 r) )	; move down
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
                    (push r openList)	; add new state to list
            )
            ((= (nth 6 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 3 p) (nth 6 p) )	; move up
                    (append openList p)
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 7 q) (nth 6 q) )	; move right
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
            )
            ((= (nth 7 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 6 p) (nth 7 p) )	; move left
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 4 q) (nth 7 q) )	; move up
                    (setf r (copy-list L) )	; make copy of list
                    (rotatef (nth 8 r) (nth 7 r) )	; move right
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
                    (push r openList)	; add new state to list
            )
            ((= (nth 8 L) 0)
                    (setf p (copy-list L) )	; make copy of list
                    (rotatef (nth 5 p) (nth 8 p) )	; move up
                    (setf q (copy-list L) )	; make copy of list
                    (rotatef (nth 7 q) (nth 8 q) )	; move left
                    (push p openList)	; add new state to list
                    (push q openList)	; add new state to list
            )
        )
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
 |   L - current state for the puzzle
 |
 |#
(defun goalState? (L)
    (let (g)
        (setf g '( 1 2 3 8 0 4 7 6 5 ) )	; set the goal state
        (if (equal L g) t nil)	; check if current state is the goal state
    )
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
                ( format t "~d ~d ~d~%" 
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                )
                ( format t "~d ~d ~d~%"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                )
                ( format t "~d ~d ~d~%~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                )
            )
        )
        
        ;If two steps left in path
        ( ( = ( length path ) 2 )
            ( let ( ( step1 ( nth 0 path ) ) ( step2 ( nth 1 path ) ) )
                ;Print two remaining steps
                ( format t "~d ~d ~d          ~d ~d ~d~%"
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                    ( nth 0 step2 ) ( nth 1 step2 ) ( nth 2 step2 )
                )
                ( format t "~d ~d ~d    ->    ~d ~d ~d~%"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                    ( nth 3 step2 ) ( nth 4 step2 ) ( nth 5 step2 )                    
                )
                ( format t "~d ~d ~d          ~d ~d ~d~%~%"
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
                ( format t "~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%"
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                    ( nth 0 step2 ) ( nth 1 step2 ) ( nth 2 step2 )
                    ( nth 0 step3 ) ( nth 1 step3 ) ( nth 2 step3 )
                )
                ( format t "~d ~d ~d    ->    ~d ~d ~d    ->    ~d ~d ~d~%"
                    ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 )
                    ( nth 3 step2 ) ( nth 4 step2 ) ( nth 5 step2 )
                    ( nth 3 step3 ) ( nth 4 step3 ) ( nth 5 step3 )                    
                )
                ( format t "~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%~%"
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
                ( format t "~d ~d ~d          ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%"
                    ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 )
                    ( nth 0 step2 ) ( nth 1 step2 ) ( nth 2 step2 )
                    ( nth 0 step3 ) ( nth 1 step3 ) ( nth 2 step3 )
                    ( nth 0 step4 ) ( nth 1 step4 ) ( nth 2 step4 )
                )
                ( format t "~d ~d ~d    ->    ~d ~d ~d    ->    ~d ~d ~d    ->    ~d ~d ~d"
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
                ( format t "~d ~d ~d          ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%~%"
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