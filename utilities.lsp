(defun generateSuccessors (L)
    (let (p q r s openList)
        (cond
            ((= (nth 0 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 1 p) (nth 0 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 3 q) (nth 0 q) )
                    (push p openList)
                    (push q openList)
            )
            ((= (nth 1 L) 0) 
                    (setf p (copy-list L) )
                    (rotatef (nth 0 p) (nth 1 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 2 q) (nth 1 q) )
                    (setf r (copy-list L) )
                    (rotatef (nth 4 r) (nth 1 r) )
                    (push p openList)
                    (push q openList)
                    (push r openList)
            )
            ((= (nth 2 L) 0)
                    (setf p (copy-list L) )	
                    (rotatef (nth 1 p) (nth 2 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 5 q) (nth 2 q) )
                    (push p openList)
                    (push q openList)
            )
            ((= (nth 3 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 0 p) (nth 3 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 4 q) (nth 3 q) )
                    (setf r (copy-list L) )
                    (rotatef (nth 6 r) (nth 3 r) )
                    (push p openList)
                    (push q openList)
                    (push r openList)
            )
            ((= (nth 4 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 1 p) (nth 4 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 3 q) (nth 4 q) )
                    (setf r (copy-list L) )
                    (rotatef (nth 5 r) (nth 4 r) )
                    (setf s (copy-list L) )
                    (rotatef (nth 7 s) (nth 4 s) )
                    (push p openList)
                    (push q openList)
                    (push r openList)
                    (push s openList)
            )
            ((= (nth 5 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 2 p) (nth 5 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 4 q) (nth 5 q) )
                    (append openList p)
                    (setf r (copy-list L) )
                    (rotatef (nth 8 r) (nth 5 r) )
                    (push p openList)
                    (push q openList)
                    (push r openList)
            )
            ((= (nth 6 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 3 p) (nth 6 p) )
                    (append openList p)
                    (setf q (copy-list L) )
                    (rotatef (nth 7 q) (nth 6 q) )
                    (push p openList)
                    (push q openList)
            )
            ((= (nth 7 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 6 p) (nth 7 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 4 q) (nth 7 q) )
                    (setf r (copy-list L) )
                    (rotatef (nth 8 r) (nth 7 r) )
                    (push p openList)
                    (push q openList)
                    (push r openList)
            )
            ((= (nth 8 L) 0)
                    (setf p (copy-list L) )
                    (rotatef (nth 5 p) (nth 8 p) )
                    (setf q (copy-list L) )
                    (rotatef (nth 7 q) (nth 8 q) )
                    (push p openList)
                    (push q openList)
            )
        )
    )
)
(defun goalState? (L)
    (let (g)
        (setf g '( 1 2 3 8 0 4 7 6 5 ) )
        (if (equal L g) t nil)
    )
)

( defun printPuzzle ( puzzle )
    ( format t "~d ~d ~d~%" ( nth 0 puzzle ) ( nth 1 puzzle ) ( nth 2 puzzle ) )
    ( format t "~d ~d ~d~%" ( nth 3 puzzle ) ( nth 4 puzzle ) ( nth 5 puzzle ) )
    ( format t "~d ~d ~d~%~%" ( nth 6 puzzle ) ( nth 7 puzzle ) ( nth 8 puzzle ) )
)

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

( defun printSolutionPath( path )
    ( cond
        ;If no path, do nothing
        ( ( = ( length path ) 0 ) )
        
        ;If one step left in path
        ( ( = ( length path ) 1 )
            ;Get steps in path
            ( let ( ( step1 ( nth 0 path ) ) )
                ;Print remaining step
                ( format t "~d ~d ~d~%" ( nth 0 step1 ) ( nth 1 step1 ) ( nth 2 step1 ) )
                ( format t "~d ~d ~d~%" ( nth 3 step1 ) ( nth 4 step1 ) ( nth 5 step1 ) )
                ( format t "~d ~d ~d~%~%" ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 ) )
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
                ( format t "~d ~d ~d          ~d ~d ~d~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                    ( nth 6 step2 ) ( nth 7 step2 ) ( nth 8 step2 )
                )
            )
        )
        
        ;If three steps left in path
        ( ( = ( length path ) 3 )
            ( let ( ( step1 ( nth 0 path ) ) ( step2 ( nth 1 path ) ) ( step3 ( nth 2 path ) ) )
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
                ( format t "~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%"
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
                ( format t "~d ~d ~d          ~d ~d ~d          ~d ~d ~d          ~d ~d ~d~%"
                    ( nth 6 step1 ) ( nth 7 step1 ) ( nth 8 step1 )
                    ( nth 6 step2 ) ( nth 7 step2 ) ( nth 8 step2 )
                    ( nth 6 step3 ) ( nth 7 step3 ) ( nth 8 step3 )
                    ( nth 6 step4 ) ( nth 7 step4 ) ( nth 8 step4 )
                )
            )
            
            ;If more than four steps were left
            ( when ( > ( length path ) 4 )
                (format t "~%")
                ;Recurse to print remaining steps
                ( printSolutionPath ( nthcdr 4 path ) )
            )
        )
    )
)

( defun printBFSResults ( path )
    ;Print results heading
    ( format t "BFS Graph Search~%" )
    ( format t "----------------~%" )
    ( format t "Solution found in ~d moves~%" ( - ( length path ) 1 ) )
    ( format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%"
        *NUM_GEN*
        *NUM_DIST*
        *NUM_EXP*
    )
    
    ;Call helper function to replace 0's in solution with spaces for output
    ( removeZeros path )

    ;Print BFS solution path
    ( printSolutionPath path )
)