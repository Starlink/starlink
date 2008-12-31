*+H_GETC           Gets Character input from terminal, or default
*  History
*     1989 Jan	M Ricketts	1st version, based on M Harris's MDH_GETC
*-    1992 Apr	M. Duesterhaus	remove VAX specific code
*     1993 Jun  P. Brisco       Added # option to skip similar fields.
*     1993 Jun  P. Brisco       Added & option to go to previous field.
*     1993 Jun  P. Brisco       Added ?? option to list menu help.
***************************************************************************
      SUBROUTINE H_GETC( QUERY, DEFAULT, CVALUE, STATUS)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) QUERY	! In	Question
      CHARACTER*(*) DEFAULT	! Default answer, or ' '
      CHARACTER*(*) CVALUE	!	 Char string returned
      INTEGER STATUS

*  Function
      INTEGER MDH_ENDWORD	! Finds end of string.
      INTEGER FIND_NOT_BLANK

*  Local Variables
      INTEGER DLEN	! Length of default.
     & ,      IERR	! Error indicator.
     & ,      LEN 	! Length of query.
     & ,      NBLANKS	! Number of blank lines at end of command.
      INTEGER I,START
      CHARACTER*60  BLANK

*  _____________________________ Executable Code _____________________________
      DO I=1,60
        BLANK(I:I) = ' '
      END DO

      IERR = 1
      DLEN = MDH_ENDWORD( DEFAULT )

      LEN = MDH_ENDWORD( QUERY )
      NBLANKS = 0

      DO WHILE ( IERR .NE. 0 )

         IF ( DLEN .EQ. 0 ) THEN
            WRITE( * , '( X,A,$ )' ) QUERY( 1:LEN )//': '
         ELSE
            WRITE( * , 1) QUERY( 1:LEN ) // ', Default is "'// DEFAULT(1:DLEN) //'": '
1           FORMAT(1X,A,$)
         END IF

         CVALUE = ' '
         READ( * , '( A )' , IOSTAT = IERR ) CVALUE

         IF (IERR.NE.0) THEN
            STATUS = 1
            IERR = 0
         ELSE IF (CVALUE .EQ. '?') THEN
            STATUS = 2
            IERR = 0

         ELSE IF (CVALUE .EQ. '%') THEN
            STATUS = 3
            IERR = 0

         ELSE IF (CVALUE .EQ. '^') THEN
            CVALUE = BLANK(1:DLEN)
            STATUS = 0
            IERR = 0

	 ELSE IF (cvalue .EQ. '#') THEN
            status = 4
            ierr = 0

         ELSE IF (cvalue .EQ. '&') THEN
            status = 5
            ierr = 0

         ELSE IF (cvalue .EQ. '??') THEN
            status = 6
            ierr = 0

	 ELSE IF (cvalue .EQ. '???') THEN
	    status = 7
	    ierr = 0

         ELSE IF ( CVALUE .EQ. ' ' .AND. DLEN .GT.0) THEN			!  If carriage return.

            CVALUE = DEFAULT							!   Default is required.
            CALL UPC(CVALUE)
            IERR = 0								!   Indicate valid answer.
            STATUS = 0

	 ELSE IF ( CVALUE .EQ. ' ' .AND. DLEN .EQ. 0) THEN
	    CVALUE = BLANK(1:)
            IERR = 0
            STATUS = 0

         ELSE									!  Else any other input.
            START = FIND_NOT_BLANK(CVALUE)
            CVALUE = CVALUE(START:)
            STATUS = 0
            CALL UPC(CVALUE)
         END IF

      END DO

      END
