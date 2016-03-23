; Node structure: stores state, parent, depth, hValue, and FValue.
#|
 | Function: AStar
 |
 | Description:
 | Runs all the different A* methods
 |
 | Parameters:
 | start - Starting state
 |
 |#
(defun AStar (start) 
	(format t "A* Search (Hamming)~%")
	(format t "-------------------~%")
	(printSearchResults (doAStar ( copy-list start ) #'tilesOutOfPlace))
	
	;Reset global variables that track statistics of search    
    ( setf *NUM_GEN*  0 )
    ( setf *NUM_DIST* 0 )
    ( setf *NUM_EXP*  0 )
	
	(format t "A* Search (Mangattan)~%")
	(format t "---------------------~%")
	(printSearchResults (doAStar ( copy-list start ) #'manhattan))
	
	;Reset global variables that track statistics of search    
    ( setf *NUM_GEN*  0 )
    ( setf *NUM_DIST* 0 )
    ( setf *NUM_EXP*  0 )
	
	(format t "A* Search (Nilsson's Sequence - inadmissible)~%")
	(format t "---------------------------------------------~%")
	(doAStar ( copy-list start ) #'nilsson)
	


)

#|
 | Function: doAStar
 |
 | Description:
 |	execute the A* algorithm using the passed in function as the h(n) 
 | Parameters:
 |   start - the start state 
 |   func - the heuristic function
 |
 |#
; Given a start state and a search type (A*), return a path from the start to the goal.
(defun doAStar (start func)
    (do*                                                             ; note use of sequential DO*
        (                                                            ; initialize local loop vars
            (curNode (make-node :state start :parent nil :depth 0 :hValue (funcall func start) :fValue (funcall func start) ))  ; current node: (start nil 0)

            (OPEN (list curNode))                                    ; OPEN list:    ((start nil 0))
            (CLOSED nil)                                             ; CLOSED list:  ( )
        )
		
        ; termination condition - return solution path when goal is found
        ((goalState? (node-state curNode)) (build-solution curNode CLOSED))
		
        ; loop body
        (when (null OPEN) (return nil))             ; no solution
        
        ; get current node from OPEN, update OPEN and CLOSED
		(when (not (= (length OPEN) 1))
			(setf OPEN (sortOpen OPEN))
		)
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
		;( format t "~%OPEN LIST~%~s~%CLOSED LIST~%~s~%" OPEN CLOSED )
        (setf CLOSED (cons curNode CLOSED))
		
        
        ; increment count of nodes expanded
        ( setf *NUM_EXP* ( 1+ *NUM_EXP* ) )

        ; add successors of current node to OPEN
        (dolist (child (generateSuccessors (node-state curNode)))

            ; for each child node
            (setf child (make-node  :state child
                                    :parent (node-state curNode)
                                    :depth (1+ (node-depth curNode))
									:hValue (funcall func child)
									:fValue 0
						)
			)
			(setf (node-fValue child) (+ (node-depth child) (node-hValue child)))
            
            ; increment number of generated nodes
            (setf *NUM_GEN* ( 1+ *NUM_GEN* ) )
			
			
			 ; if the node is on OPEN but child is better
			(when (member child OPEN :test #'equal-states)
				
				(setf oldNode (car (member child OPEN :test #'equal-states)))
                ; remove old node and add child to the OPEN list
                (when (< (node-fValue child) (node-fValue oldNode))

                    ; A - add to end of OPEN list (queue)
					(setf OPEN (remove oldNode OPEN :test #'equal-states))
					(setf OPEN (append OPEN (list child)))
					( setf *NUM_GEN* ( 1- *NUM_GEN* ) )
				)
					

                
			)
			
			; if the node is on CLOSED but child is better
            (when (member child CLOSED :test #'equal-states)
				
				(setf oldNode (car (member child CLOSED :test #'equal-states)))
                ; remove old node and add child to the OPEN list
                (cond

                    ; remove from old CLOSED and add to child OPEN list
                    ((< (node-fValue child) (node-fValue oldNode))
						(setf CLOSED (remove oldNode CLOSED :test #'equal-states))
						(setf OPEN (append OPEN (list child)))
						( setf *NUM_GEN* ( 1- *NUM_GEN* ) )
					)

                )
            )
			
            ; if the node is not on OPEN or CLOSED
            (when   (and (not (member child OPEN   :test #'equal-states))
                         (not (member child CLOSED :test #'equal-states))
                    )
                
                ; increment number of distinct nodes
                ( setf *NUM_DIST* ( 1+ *NUM_DIST* ) )

                ; add it to the OPEN list
                

                ; A - add to end of OPEN list (queue)
                (setf OPEN (append OPEN (list child)))
                
            )
        )
    )
)

#|
 | Function: sortOpen
 |
 | Description: places the node with the lowest
 | f(n) value at the front of the open list.
 |
 | Parameters:
 |   OPEN - the open list of nodes
 |
 |#
(defun sortOpen (OPEN)
(let (lowest)
	(setf lowest (car OPEN))
	(dolist (i OPEN)
		(when (< (node-fValue i) (node-fValue lowest) )
			(setf lowest i)
		)
	)
	(setf OPEN (remove lowest OPEN :test #'equal-states))
	(cons lowest OPEN)
)
)

#|
 | Function: tilesOutOfPlace
 |
 | Description:
 |  Counts the number of tiles out of place in
 |  the passed in state when compared to the goal
 |  
 | Parameters:
 |   state- current state of the puzzle
 |
 |#
(defun tilesOutOfPlace (state)
; ( 1 2 3 8 0 4 7 6 5 )
	(let (count)
		(setf count 0)
		
		(when (not (= (nth 0 state) 1))
			(incf count)
		)
		(when (not (= (nth 1 state) 2))
			(incf count)
		)
		(when (not (= (nth 2 state) 3))
			(incf count)
		)
		(when (not (= (nth 3 state) 8))
			(incf count)
		)
		(when (not (= (nth 4 state) 0))
			(incf count)
		)
		(when (not (= (nth 5 state) 4))
			(incf count)
		)
		(when (not (= (nth 6 state) 7))
			(incf count)
		)
		(when (not (= (nth 7 state) 6))
			(incf count)
		)
		(when (not (= (nth 8 state) 5))
			(incf count)
		)
		
		(return-from tilesOutOfPlace count)
	)
)

#|
 | Function: manhattan
 |
 | Description:
 |  heuristic function that measures the distance a tile is from
 |  its goal position and totals the valuess into a sum
 |
 | Parameters:
 |   state - current state of the puzzle
 |
 |#
(defun manhattan (state)
;  ( 1 2 3 8 0 4 7 6 5 )
(let (rowList rowState sum row col is at curc curr pair count)
	
	(setf sum 0)
	(setf count 0)
	(setf rowList '( 1 2 3 8 0 4 7 6 5 ))
	(setf rowState state)
	(setf curc 1)
	(setf curr 1)
	
	(dolist (i rowState)
		(incf count)
		(setf row 1)
		(setf col 1)
		;(format t "col row ~s ~s~%" col row)
		(dolist (j rowList)
			(when (= i j)
				(setf is nil)
				(setf is (list col row))
				(setf at nil)
				(setf at (list curc curr))
				(setf pair (mapcar #'abs (mapcar #'- is at)))
				(setf sum (+ sum (+ (car pair) (car (cdr pair)))))
				(return)
				
				
				
			)
			(when (= (mod col 3) 0)
				(setf col 0)
				(incf row)
			)
			(when (= (mod row 4) 0)
				(setf row 1)
			)
			(incf col)
			
		)
		
		(when (= curc 3)
			(setf curc 0)
			(incf curr)
		)
		(incf curc)
		(when (= count (length state))
		
			(return-from manhattan sum)
		
		)
	)
)
)

#|
 | Function: nilsson
 |
 | Description:
 |  inadmissible heuristic function:
 |  h(n) = P(n) + 3S(n)
 |
 |  P(n) is the Manhattan Distance of each tile from its proper position.
 |  S(n) is the sequence score obtained by checking around the non-central squares in turn,
 |  allotting 2 for every tile not followed by its proper successor and 1 
 |  in case that the center is not empty. 
 |
 | Parameters:
 |   state - current state of the puzzle
 |
 |#
(defun nilsson (state)
(let (goal MDist actual sum next)
	(setf sum 0)
	(setf goal '( 1 2 3 8 0 4 7 6 5 ))
	(setf MDist (manhattan state))
	(setf actual 0)
	(setf next 0)
	
	(dolist (val state)
		(setf next (second (member val state)))
		(when (equal next nil)
			(setf next (length state))
		)
		(setf actual (second (member val goal)))
		(when (equal actual nil)
			(setf actual (length state))
		)
		(when (and (= next 0) (not (= actual 0)))
			(incf sum)
		)
		(when (not (= next actual))
			(setf sum (+ sum 2))
		)
	
	)
	(setf sum (+ (* sum 3) MDist))
	
	(return-from nilsson sum)
	


)
)