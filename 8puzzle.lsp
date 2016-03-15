( defun 8puzzle ( &optional ( puzzle NIL ) )
    ;Load program files
    ( load 'read_goal )
    ;( load 'utilities )
    
    ;Load puzzle file
    ;( get_puzzle args )
    
    ( format t "~s~%" puzzle )
)

( defun get_puzzle_file ( args )
    ( cond 
        ;If exactly one command line argument given
        ( ( = 1 ( length args ) )
            ;Get puzzle from specified file
            ( readPuzzleFile ( car args ) )
        )
        
        ;If not exactly one command line argument given
        ( t
            ;Required command line argument missing, print usage statement
            (format t "Usage: clisp 8puzzle.lsp puzzlefile~%")
            ;(format t "       clisp - runs clisp interpetter~%")
            ;(format t "       8puzzle.lsp - program file~%")
            ;(format t "       puzzlefile - file containing the postions of the numbers in the puzzle~%")
        )
    )
)

;Script commands for program run from command line
( load 'read_goal )
( 8puzzle ( get_puzzle_file *ARGS* ) )