*+
*-  Author M.D.C.Harris ( R.A.L )                    11th June 1987.
*   16 Apr 1992		M. Duesterhaus		Remove VAX specific code
**************************************************************************
      CHARACTER*(*) FUNCTION MDH_GETC( QUERY , DEFAULT )

*  -----------
*  DESCRIPTION
*  -----------

*  Asks a question and returns a character input from terminal using the
* default if nothing entered.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) DEFAULT	! Default answer.
     & ,            QUERY       ! Question.

*LOCAL:

      INTEGER I        	! Number of blank lines at start of command.
     & ,      DLEN	! Length of default.
     & ,      IERR	! Error indicator.
     & ,      LEN 	! Length of query.
     & ,      NBLANKS	! Number of blank lines at end of command.

*  -------------------
*  FUNCTION REFERENCED
*  -------------------

      INTEGER MDH_ENDWORD	! Finds end of string.

      IERR = 1
      DLEN = MDH_ENDWORD( DEFAULT )
      I = 1

      DO WHILE ( QUERY( I:I ) .EQ. '/' )

        WRITE( * , * )
        I = I + 1

      END DO

      LEN = MDH_ENDWORD( QUERY )
      NBLANKS = 0

      DO WHILE ( LEN .GT. 0 .AND. QUERY( LEN:LEN ) .EQ. '/' )

        NBLANKS = NBLANKS + 1
        LEN = LEN - 1

      END DO

      DO WHILE ( IERR .NE. 0 )

        IF ( DLEN .EQ. 0 ) THEN

          WRITE( * , '( X,A,$ )' ) QUERY( I:LEN ) // ': '

        ELSE

          WRITE( * , '( X,A,$ )' )
     &           QUERY( I:LEN ) // '\ Default is ' // DEFAULT(1:DLEN) // '\: '

        END IF

        READ( * , '( A )' , IOSTAT = IERR ) MDH_GETC

      END DO

      IF ( MDH_GETC .EQ. ' ' ) MDH_GETC = DEFAULT

      DO I = 1 , NBLANKS

        WRITE( * , * )

      END DO

      END
