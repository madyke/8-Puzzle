( defun 8puzzle ( &optional ( puzzle NIL ) )
    ;Load program files
    ( load 'search)
    ( load 'utilities )

    ;Debug
    ( printPuzzle puzzle )
    ( mapcar #'printPuzzle ( generateSuccessors puzzle '() ) )
)

;Script commands for program when run from command line
( load 'readPuzzle )
( 8puzzle ( get_puzzle_file *ARGS* ) )