; Node structure: stores state, parent, depth, hValue, and FValue.
(defun AStar (start) (doAStar ( copy-list start ) 'A))

; Given a start state and a search type (A*), return a path from the start to the goal.
(defun doAStar (start type)
    (do*                                                             ; note use of sequential DO*
        (                                                            ; initialize local loop vars
            (curNode (make-node :state start :parent nil :depth 0 :hValue (tilesOutOfPlace start) :fValue (tilesOutOfPlace start) ))  ; current node: (start nil 0)

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
									:hValue (tilesOutOfPlace child)
									:fValue 0
						)
			)
			;( format t "CHILD: ~s FValue: ~s~%" (node-depth child) (node-hValue child))
			(setf (node-fValue child) (+ (node-depth child) (node-hValue child)))
            ;( format t "CHILD: ~s FValue: ~s~%" child (node-fValue child))
            
            ; increment number of generated nodes
            (setf *NUM_GEN* ( 1+ *NUM_GEN* ) )
			
			
			 ; if the node is on OPEN but child is better
			(when (member child OPEN :test #'equal-states)
				
				(setf oldNode (car (member child OPEN :test #'equal-states)))
                ; remove old node and add child to the OPEN list
                (when (< (node-fValue child) (node-fValue oldNode))

                    ; A - add to end of OPEN list (queue)
					(format t "here~%")
					(setf OPEN (remove oldNode 'OPEN :test #'equal-states))
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
						(format t "closed here~%")
						(setf CLOSED (remove oldNode 'CLOSED :test #'equal-states))
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
                (cond

                    ; A - add to end of OPEN list (queue)
                    ((eq type 'A) (setf OPEN (append OPEN (list child))))

                    ; error handling for incorrect usage
                    (t (format t "SEARCH: bad search type! ~s~%" type) (return nil))
                )
            )
        )
    )
)

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