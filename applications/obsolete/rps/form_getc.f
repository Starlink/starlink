*+FORM_GETC        Gets character string from Terminal-user
*     DATE		AUTHOR			DESCRIPTION
*-    11 Jun 1987	M.D.C.Harris ( R.A.L )	original
*     ???		MJR 			for screen alternative
*     8 Apr 1992	M. Duesterhaus (GSFC)	remove VAX RTL calls
*     1993 Jun          P. Brisco               Removed SMG junque.
***************************************************************************
      CHARACTER*(*) FUNCTION FORM_GETC( QUERY , DEFAULT)
      IMPLICIT NONE

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

*Global
      INTEGER KPID,PBID
      COMMON /SMG_PANDK/ PBID,KPID

*LOCAL:

      INTEGER I        	! Number of blank lines at start of command.
     & ,      DLEN	! Length of default.
     & ,      IERR	! Error indicator.
     & ,      LEN 	! Length of query.
     & ,      NBLANKS	! Number of blank lines at end of command.
      INTEGER LOC

*  -------------------
*  FUNCTION REFERENCED
*  -------------------

      INTEGER MDH_ENDWORD	! Finds end of string.

      IERR = 1
      DLEN = MDH_ENDWORD( DEFAULT )
      LEN = MDH_ENDWORD( QUERY )


      I = 1
      DO WHILE (QUERY(I:I) .EQ. '/' )		! Leading blank lines
         I = I+1
      END DO

      NBLANKS = 0
      DO WHILE ( LEN .GT. 0 .AND. QUERY( LEN:LEN ) .EQ. '/' )	! Trailing blank lines

        NBLANKS = NBLANKS + 1
        LEN = LEN - 1

      END DO
      LOC = INDEX(QUERY(I:LEN), '/' )				! Use / to put line-feed, can o/p longer string


         DO WHILE ( IERR .NE. 0 )

            IF (LOC .NE. 0) THEN					! Write 1st line
               WRITE( * , '(A)' ) QUERY(I:LOC-1)
               I = LOC + 1
            END IF

           IF ( DLEN .EQ. 0 ) THEN

             WRITE( * , '( X,A,$ )' ) QUERY( I:LEN )//': '

           ELSE

             WRITE( * , '( X,A,$ )' )
     &           QUERY( I:LEN ) // '( Default is ' // DEFAULT(1:DLEN) // '): '

           END IF

           READ( * , '( A )' , IOSTAT = IERR ) FORM_GETC

         END DO


         DO I = 1 , NBLANKS

           WRITE( * , * )

         END DO

      IF ( FORM_GETC .EQ. ' ' ) FORM_GETC = DEFAULT

      END
