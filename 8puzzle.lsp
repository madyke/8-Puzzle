( defun 8puzzle ( &optional ( puzzle NIL ) )
    
    ;DEBUG
    ( format t "~s~%" puzzle )
    
    ;Load program files
    ( load 'read_goal )
    ;( load 'utilities )
    
    ;Load puzzle file
    ;( get_puzzle args )
)

;Script commands for program run from command line
( load 'read_goal )
( 8puzzle ( get_puzzle_file *ARGS* ) )