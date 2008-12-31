*+
      CHARACTER*(*) FUNCTION MDH_GETCN( QUERY )

*  -----------
*  DESCRIPTION                                                 
*  -----------

*  Asks a question and returns a character input from the terminal.

*  ---------
*  VARIABLES
*  ---------

*INPUT: 

      CHARACTER*(*) QUERY	! Question.

*LOCAL:

      INTEGER       I		! Loop variable.
     & ,            IERR	! Error indicator.
     & ,            LEN 	! Length of query.
     & ,            NBLANKS	! Number of blank lines at end of command.

*  -------------------
*  FUNCTION REFERENCED
*  -------------------

      INTEGER MDH_ENDWORD	! Finds end of string.

*-  Author M.D.C.Harris ( R.A.L )                    10th June 1987.

      IERR = 1
      NBLANKS = 0
      LEN = MDH_ENDWORD( QUERY )

      DO WHILE ( LEN .GT. 0 .AND. QUERY( LEN:LEN ) .EQ. '/' )

        NBLANKS = NBLANKS + 1
        LEN = LEN - 1

      END DO

      DO WHILE ( IERR .NE. 0 )

        WRITE( * , '( X,A,$ )' ) QUERY( :LEN )//': '
        READ ( * , '( A )' , IOSTAT = IERR ) MDH_GETCN
 
      END DO

      DO I = 1 , NBLANKS

        WRITE( * , * )

      END DO

      END
