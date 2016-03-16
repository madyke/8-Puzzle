( defun 8puzzle ( &optional ( puzzle NIL ) )
    ;Load program files
    ;( load 'utilities )
)

;Script commands for program run from command line
( load 'readPuzzle )
( 8puzzle ( get_puzzle_file *ARGS* ) )