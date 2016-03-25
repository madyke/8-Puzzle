#|
 | Function: moveDown
 |
 | Description: This function moves the blank (zero) tile
 | down one spot.
 |
 | Parameters:
 |   L - current state for the puzzle
 |   pos - the position in the puzzle that contains zero
 |
 |#
(defun moveDown (L pos)
	(let (p openList)
		(cond
			((= (listLength L) 9)	;if this is an 8 puzzle
				(setf p (copy-list L))	; make a copy of the current state
				(rotatef (nth (+ pos 3) p) (nth pos p) )	; move blank spot down
				(push p openList)	; add new state onto the list
			)
			((= (listLength L) 16)	;if this is a 15 puzzle
				(setf p (copy-list L))	; make a copy of the current state
				(rotatef (nth (+ pos 4) p) (nth pos p) )	; move blank spot down
				(push p openList)	; add new state onto the list
			)
			((= (listLength L) 25)	; if this is a 24 puzzle
				(setf p (copy-list L))	; make a copy of the current state
				(rotatef (nth (+ pos 5) p) (nth pos p) )	; move blank spot down
				(push p openList)	; add new state onto the list
			)
		)
	)
)

#|
 | Function: moveUp
 |
 | Description: This function moves the blank (zero) tile
 | up one spot.
 |
 | Parameters:
 |   L - current state for the puzzle
 |   pos - the position in the puzzle that contains zero
 |
 |#
(defun moveUp (L pos)
	(let (p openList)
		(cond
			((= (listLength L) 9)	; if this is an 8 puzzle
				(setf p (copy-list L))	; make a copy of the current state
				(rotatef (nth (- pos 3) p) (nth pos p) )	; move blank spot up
				(push p openList)	; add new state onto the list
			)
			((= (listLength L) 16)	; if this is a 15 puzzle
				(setf p (copy-list L))	; make a copy of the current state
				(rotatef (nth (- pos 4) p) (nth pos p) )	; move blank spot up
				(push p openList)	; add new state onto the list
			)
			((= (listLength L) 25)	; if this is a 24 puzzle
				(setf p (copy-list L))	; make a copy of the current state
				(rotatef (nth (- pos 5) p) (nth pos p) )	; move blank spot up
				(push p openList)	; add new state onto the list
			)
		)
	)
)

#|
 | Function: moveLeft
 |
 | Description: This function moves the blank (zero) tile
 | left one spot.
 |
 | Parameters:
 |   L - current state for the puzzle
 |   pos - the position in the puzzle that contains zero
 |
 |#
(defun moveLeft (L pos)
	(let (p openList)
		(setf p (copy-list L))	; make a copy of the current state
		(rotatef (nth (- pos 1) p) (nth pos p) )	; move blank spot left
		(push p openList)	; add new state onto the list
	)
)

#|
 | Function: moveRight
 |
 | Description: This function moves the blank (zero) tile
 | right one spot.
 |
 | Parameters:
 |   L - current state for the puzzle
 |   pos - the position in the puzzle that contains zero
 |
 |#
(defun moveRight (L pos)
	(let (p openList)
		(setf p (copy-list L))	; make a copy of the current state
		(rotatef (nth (+ pos 1) p) (nth pos p) )	; move blank spot right
		(push p openList)	; add new state onto the list
	)
)

#|
 | Function: listLength
 |
 | Description: This function finds the length of the 
 | current list.
 |
 | Parameters:
 |   L - current state for the puzzle
 |
 |#
(defun listLength (L)
    (let ((len 0))	; set len initally to zero
        (dolist (i L len)	; loop through list
            (incf len)	; increment len
        )
    )
)

#|
 | Function: goalState?
 |
 | Description: This funciton determines if the current puzzle
 | is the goal state. Below are the goal states for the 8, 15, 
 | and 24 puzzles.
 |
 | 8-puzzle		  15-puzzle			    24-puzzle
 | 1  2  3		1   2   3   4		1   2   3   4   5
 | 8  0  4		12  13  14  5		16  17  18  19  6
 | 7  6  5		11  0   15  6		15  24  0   20  7
 |				10  9   8   7		14  23  22  21  8
 |									13  12  11  10  9
 |
 | Parameters:
 |   L - current state for the puzzle
 |
 |#
(defun goalState? (L)  
    (let (g)
		(cond
			((= (listLength L) 9)	; if this is an 8 puzzle
				(setf g '( 1 2 3 8 0 4 7 6 5 ) ) ; set the goal state
				(if (equal L g) t nil) ; check if current state is the goal state
			)
			((= (listLength L) 16)	; if this is a 15 puzzle
				(setf g '( 1 2 3 4 12 13 14 5 11 0 15 6 10 9 8 7 ) )	; set the goal state
				(if (equal L g) t nil) ; check if current state is the goal state
			)
			((= (listLength L) 25)	; if this is a 24 puzzle
				(setf g '( 1 2 3 4 5 16 17 18 19 6 15 24 0 20 7 14 23 22 21 8 13 12 11 10 9 ) ) ; set the goal state
				(if (equal L g) t nil)	; check if current state is the goal state
			)
		)
    )
)

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
	(let (pos len openList)
		(setf pos (position 0 L))	; set the position of zero in the list
		(setf len (listLength L))	; set the length of the list
		(cond
			; if  blank is in top right, generate successors
			((or (= pos 0))		
					(push (moveDown L pos) openList)	; move blank down
					(push (moveRight L pos) openList)	; move blank right
			)
			; if the blank is in the bottom left
			((or (= pos 20) (and (= len 9) (= pos 6)) (and (= len 16) (= pos 12)))
					(push (moveUp L pos) openList)	; move blank up
					(push (moveRight L pos) openList)	; move blank right
			)
			; if the blank is in the top right 
			((or (and (= len 9) (= pos 2)) 
				 (and (= len 16) (= pos 3)) 
				 (and (= len 25) (= pos 4)))
					(push (moveDown L pos) openList)	; move blank down
					(push (moveLeft L pos) openList)	; move blank left
			)
			; if the blank is in the bottom right 
			((or (= pos 24) (and (= len 9) (= pos 8)) (and (= len 16) (= pos 15)))
				(push (moveUp L pos) openList)	; move blank up
				(push (moveLeft L pos) openList)	; move blank left
			)
			; if the blank is against the top of the puzzle and not in a corner
			((or (= pos 1) (and (> len 9) (= pos 2)) (and (= len 25) (= pos 3))) 
					(push (moveDown L pos) openList)	; move blank down
					(push (moveRight L pos) openList)	; move blank right
					(push (moveLeft L pos) openList)	; move blank left
			)
			; if the blank is against the bottom of the puzzle and not in a corner
			((or (and (= len 9) (= pos 7)) 
				 (and (= len 16) 
				 (or (= pos 13) (= pos 14))))  
					(push (moveUp L pos) openList)	; move blank up
					(push (moveRight L pos) openList)	; move blank right
					(push (moveLeft L pos) openList)	; move blank left
			)
			; if the blank is against the left side and not in a corner
			((or (and (= len 9) (= pos 3)) 
				 (and (= len 16) (or (= pos 4) (= pos 8))) 
				 (and (= len 25) (or (= pos 5) (= pos 10) (= pos 15)))) 
					(push (moveDown L pos) openList)	; move blank down
					(push (moveRight L pos) openList)	; move blank right
					(push (moveUp L pos) openList)	; move blank up
			)
			; if the blank is against the right side and not in a corner
			((or (= pos 19) 
				 (and (= len 9) (= pos 5)) 
				 (and (= len 16) (or (= pos 7) (= pos 11))) 
				 (and (= len 25) (or (= pos 9) (= pos 14)))) 
					(push (moveDown L pos) openList)	; move blank down
					(push (moveUp L pos) openList)	; move blank up
					(push (moveLeft L pos) openList)	; move blank left
			)
			; if the blank can be moved in all four directions
			((or (and (= len 9) (= pos 4)) 
				 (and (= len 16) (or (= pos 5) (= pos 6) (= pos 9) (= pos 10))) 
				 (and (= len 25) (or (= pos 7) (= pos 8) (= pos 11) (= pos 12) (= pos 13))) 
				 (= pos 16) (= pos 17) (= pos 18)) 
					(push (moveDown L pos) openList)	; move blank down
					(push (moveRight L pos) openList)	; move blank right
					(push (moveLeft L pos) openList)	; move blank left
					(push (moveUp L pos) openList)	; move blank up
			)
		)
	)
)