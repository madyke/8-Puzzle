(defun moveDown (L pos)
	(let (p openList)
		(cond
			((= (listLength L) 9)
				(setf p (copy-list L))
				(rotatef (nth (+ pos 3) p) (nth pos p) )
				(push p openList)
			)
			((= (listLength L) 16)
				(setf p (copy-list L))
				(rotatef (nth (+ pos 4) p) (nth pos p) )
				(push p openList)
			)
			((= (listLength L) 25)
				(setf p (copy-list L))
				(rotatef (nth (+ pos 5) p) (nth pos p) )
				(push p openList)
			)
		)
	)
)

(defun moveUp (L pos)
	(let (p openList)
		(cond
			((= (listLength L) 9)
				(setf p (copy-list L))
				(rotatef (nth (- pos 3) p) (nth pos p) )
				(push p openList)
			)
			((= (listLength L) 16)
				(setf p (copy-list L))
				(rotatef (nth (- pos 4) p) (nth pos p) )
				(push p openList)
			)
			((= (listLength L) 25)
				(setf p (copy-list L))
				(rotatef (nth (- pos 5) p) (nth pos p) )
				(push p openList)
			)
		)
	)
)

(defun moveLeft (L pos)
	(let (p openList)
		(setf p (copy-list L))
		(rotatef (nth (- pos 1) p) (nth pos p) )
		(push p openList)
	)
)

(defun moveRight (L pos)
	(let (p openList)
		(setf p (copy-list L))
		(rotatef (nth (+ pos 1) p) (nth pos p) )
		(push p openList)
	)
)

(defun listLength (L)
    (let ((len 0))
        (dolist (i L len)
            (incf len)
        )
    )
)

(defun goalState? (L)  
    (let (g)
		(cond
			((= (listLength L) 9)
				(setf g '( 1 2 3 8 0 4 7 6 5 ) )
				(if (equal L g) t nil)
			)
			((= (listLength L) 16)
				(setf g '( 1 2 3 4 12 13 14 5 11 0 15 6 10 9 8 7 ) )
				(if (equal L g) t nil)
			)
			((= (listLength L) 25)
				(setf g '( 1 2 3 4 5 16 17 18 19 6 15 24 0 20 7 14 23 22 21 8 13 12 11 10 9 ) )
				(if (equal L g) t nil)
			)
		)
    )
)

(defun generateSuccessors (L)
	(let (pos len openList)
		(setf pos (position 0 L))
		(setf len (listLength L))
		(cond
			((or (= pos 0))
				(push (moveDown L pos) openList)
				(push (moveRight L pos) openList)
			)
			((or (= pos 20) (and (= len 9) (= pos 6)) (and (= len 16) (= pos 12)))
				(push (moveUp L pos) openList)
				(push (moveRight L pos) openList)
			)
			((or (and (= len 9) (= pos 2)) (and (= len 16) (= pos 3)) (and (= len 25) (= pos 4)))
				(push (moveDown L pos) openList)
				(push (moveLeft L pos) openList)
			)
			((or (= pos 24) (and (= len 9) (= pos 8)) (and (= len 16) (= pos 15)))
				(push (moveUp L pos) openList)
				(push (moveLeft L pos) openList)
			)
			((or (= pos 1) (and (> len 9) (= pos 2)) (and (= len 25) (= pos 3))) 
					(push (moveDown L pos) openList)
					(push (moveRight L pos) openList)
					(push (moveLeft L pos) openList)
			)
			((or (and (= len 9) (= pos 7)) (and (= len 16) (or (= pos 13) (= pos 14))))  
					(push (moveUp L pos) openList)
					(push (moveRight L pos) openList)
					(push (moveLeft L pos) openList)
			)
			((or (and (= len 9) (= pos 3)) (and (= len 16) (or (= pos 4) (= pos 8))) (and (= len 25) (or (= pos 5) (= pos 10) (= pos 15)))) 
					(push (moveDown L pos) openList)
					(push (moveRight L pos) openList)
					(push (moveUp L pos) openList)
			)
			((or (= pos 19) (and (= len 9) (= pos 5)) (and (= len 16) (or (= pos 7) (= pos 11))) (and (= len 25) (or (= pos 9) (= pos 14)))) 
					(push (moveDown L pos) openList)
					(push (moveUp L pos) openList)
					(push (moveLeft L pos) openList)
			)
			((or (and (= len 9) (= pos 4)) (and (= len 16) (or (= pos 5) (= pos 6) (= pos 9) (= pos 10))) (and (= len 25) (or (= pos 7) (= pos 8) (= pos 11) (= pos 12) (= pos 13))) (= pos 16) (= pos 17) (= pos 18)) 
					(push (moveDown L pos) openList)
					(push (moveRight L pos) openList)
					(push (moveLeft L pos) openList)
					(push (moveUp L pos) openList)
			)
		)
	)
)