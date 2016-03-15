( defun 8puzzle ()
    ;Load program files
    ( load 'read_goal )
    ;( load 'utilities )
    
    ;Load puzzle file
    (read_start *ARGS*)
)

; Call 8puzzle function if file run
(8puzzle)