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
(let (goal)

	; gets goal state as list
	(setf goal (getGoal start))
	
	(format t "A* Search (Hamming)~%")
	(format t "-------------------~%")
	(printSearchResults (doAStar ( copy-list start ) #'tilesOutOfPlace goal))
	
	;Reset global variables that track statistics of search    
    ( setf *NUM_GEN*  0 )
    ( setf *NUM_DIST* 0 )
    ( setf *NUM_EXP*  0 )
	
	(format t "A* Search (Manhattan)~%")
	(format t "---------------------~%")
	(printSearchResults (doAStar ( copy-list start ) #'manhattan goal))
	
	;Reset global variables that track statistics of search    
    ( setf *NUM_GEN*  0 )
    ( setf *NUM_DIST* 0 )
    ( setf *NUM_EXP*  0 )
	
	(format t "A* Search (Nilsson's Sequence - inadmissible)~%")
	(format t "---------------------------------------------~%")
	(doAStar ( copy-list start ) #'nilsson goal)
	

)
)

#|
 | Function: doAStar
 |
 | Description:
 |	execute the A* algorithm using the passed in function as the h(n) 
 | Parameters:
 |   start - the start state 
 |   func - the heuristic function
 |   Goal - goal state for puzzle
 |
 |#
; Given a start state and a search type (A*), return a path from the start to the goal.
(defun doAStar (start func goal)
    (do*                                                             ; note use of sequential DO*
        (                                                            ; initialize local loop vars
            (curNode (make-node :state start :parent nil :depth 0 :hValue (funcall func start goal) :fValue (funcall func start goal) ))  ; current node: (start nil 0)

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
									:hValue (funcall func child goal)
									:fValue 0
						)
			)
			; calculate nodes fValue f(n) = tree depth + heuristic value
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
 |   Goal - goal state for puzzle
 |
 |#
(defun tilesOutOfPlace (state goal)
; ( 1 2 3 8 0 4 7 6 5 ) goal state for 8 puzzle
	(let (count)
		(setf count 0)
		
		(dotimes (i (length state) count)
			(when (not (= (nth i state) (nth i goal) ) )
				(incf count)
			)
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
 |   Goal - goal state for puzzle
 |
 |#
(defun manhattan (state goal)
(let (rowList rowState sum row col is at curc curr pair count)
	
	(setf sum 0)
	(setf count 0) ; when to stop
	(setf rowList goal) ; goal state
	(setf rowState state)
	(setf curc 1) ; current column in state
	(setf curr 1) ; current row in state
	
	(dolist (i rowState)
		(incf count)
		(setf row 1) ; current row in goal state
		(setf col 1) ; current column in goal state
		(dolist (j rowList) ; parse the list until matching element found
			(when (= i j) ; when found subtract col and row to get distance from actual
				(setf is nil)
				(setf is (list col row))
				(setf at nil)
				(setf at (list curc curr))
				(setf pair (mapcar #'abs (mapcar #'- is at))) ; distance between tiles
				(setf sum (+ sum (+ (car pair) (car (cdr pair)))))  ; add to sum
				(return)
				
				
				
			)
			(when (= (mod col 3) 0) ; every three columns reset and add 1 to row
				(setf col 0)
				(incf row)
			)
			(when (= (mod row 4 ) 0) ; after rows processed reset
				(setf row 1)
			)
			(incf col)
			
		)
		
		(when (= curc 3) ; same process for state columns
			(setf curc 0)
			(incf curr)
		)
		(incf curc)
		(when (= count (length state)) ; when tile count = total tiles return
		
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
 |   Goal - goal state for puzzle
 |
 |#
(defun nilsson (state goal)
(let (MDist actual sum next)
	;h(n) = p(n) + 3s(n)
	(setf sum 0)
	(setf MDist (manhattan state goal)) ;get manhattan dist. (p(n))
	(setf actual 0) ; what next tile should be (goal)
	(setf next 0) ; what next tile is in passed in state
	
	(dolist (val state) ; for each number in state
		(setf next (second (member val state))) ; get next value
		(when (equal next nil) ; when at end of list handle "nil not a number error"
			(setf next (length state))
		)
		(setf actual (second (member val goal))) ; find value in goal and get following number
		(when (equal actual nil) ; handle nil
			(setf actual (length state))
		)
		(when (and (= next 0) (not (= actual 0))) ; if zero is in the wrong spot add one to sum
			(incf sum)
		)
		(when (not (= next actual)) ; if the next sequential tile is not correct add two to sum
			(setf sum (+ sum 2))
		)
	
	)
	; sum = s(n)
	(setf sum (+ (* sum 3) MDist))
	
	;sum = h(n) return
	(return-from nilsson sum)
	
)
)

#|
 | Function: getGoal
 |
 | Description:
 | returns a list of the goal state. 
 |
 | Parameters:
 |   L - current state of the puzzle
 |
 |#
(defun getGoal (L)  
    (let (g)
		; get spiral array and convert to a list
		(setf g '(1 2 3 8 0 4 7 6 5))
		(return-from getGoal g)
    )
)

#|
 | Function: spiral
 |
 | Author: https://rosettacode.org/wiki/Spiral_matrix#Common_Lisp 
 |
 | Description:
 | returns a list of the goal state for the n-puzzle in spiral format.
 | Taken and modified to start at one from authir above
 |
 | Parameters:
 |   L - current state of the puzzle
 |
 
(defun spiral (rows columns)
  (do ((N (* rows columns))
       (spiral (make-array (list rows columns) :initial-element nil))
       (dx 1) (dy 0) (x 0) (y 0)
       (i 1 (1+ i)))
      ((= i N) spiral)
    (setf (aref spiral y x) i)
    (let ((nx (+ x dx)) (ny (+ y dy)))
      (cond
       ((and (< -1 nx columns)
             (< -1 ny rows)
             (null (aref spiral ny nx)))
        (setf x nx
              y ny))
       (t (psetf dx (- dy)
                 dy dx)
          (setf x (+ x dx)
                y (+ y dy)))
				))))
				
	|#

#|
 | Function: getSize
 |
 | Description:
 |  finds and returns the demension of the puzzle
 |  
 | Parameters:
 |   puzzle - current state of the puzzle
 |
 
(defun getSize (puzzle)  
    (let (size)
		(dotimes (i (length puzzle) size)
			(when (not (equal i 0))
				(when (equal (* i i) (length puzzle))
					(setf *N_Rows* i)
					(setf *N_Cols* i)
					(setf size i)
					(return-from getSize size)
				)
			)
		)
		(format t "Error please provide n x n puzzle")
    )
)
|#