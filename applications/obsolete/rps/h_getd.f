*+H_GETD           Gets double-precn. input from terminal, or default
*  History
*     1989 Jan	M Ricketts	1st version, based on M Harris's MDH_GETD
*     1992 Apr  M. Duesterhaus  Remove VAX specific code
*     1993 Jun  P. Brisco       Added # option to skip similar fields.
*     1993 Jun  P. Brisco       Added & option to go to previous field.
*     1993 Jun  P. Brisco       Added ?? option to list key help.
*     1993 Jun	P. Brisco	Added ??? option to list menu help.
***************************************************************************
      SUBROUTINE H_GETD(QUERY, DEFIN, DEFAULT, DVALUE, STATUS)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) QUERY	! In	Question.
      LOGICAL DEFIN		! 	True if default
      DOUBLE PRECISION DEFAULT		! 	Default value
      DOUBLE PRECISION DVALUE		! Out	Returned value
      INTEGER STATUS		!	0: OK, 1: EOF, 2: Need help

*  Functions
      INTEGER MDH_ENDWORD	! Finds end of string.
 
*  Local Variables
      CHARACTER*64 DEFSTRING
      INTEGER NCHS
      CHARACTER*20  ANSWER    	! Input answer.
      INTEGER       I		! Loop variable.
     & ,            IERR	! Error indicator.
     & ,            PLACE	! Position of decimal point.
     & ,            LEN 	! Length of query.
      LOGICAL       DOT		! Indicates if decimal point exists.

* _____________________________ Executable Code ______________________________
 
      IF (DEFIN) THEN
         WRITE( DEFSTRING ,'(A,F11.5 )' ) ', Default is ', DEFAULT
         NCHS = 24
      ELSE
         DEFSTRING = ' '
         NCHS = 1
      END IF

      LEN = MDH_ENDWORD( QUERY )

      WRITE( * ,'( X,A,X,A,$)' ) QUERY( :LEN ) , DEFSTRING(:NCHS)//' : '
      IERR = 1

      DO WHILE ( IERR .NE. 0 )

         ANSWER = ' '
         READ( * , '( A )' ,IOSTAT=IERR ) ANSWER				!  Get answer.
         IF (IERR.NE.0) THEN
            STATUS = 1
            IERR = 0
         ELSE IF (ANSWER .EQ. '?') THEN
            STATUS = 2
            IERR = 0

         ELSE IF (ANSWER .EQ. '%') THEN
            STATUS = 3
            IERR = 0

	 ELSE IF (answer .EQ. '#') THEN
            status = 4
            ierr = 0

         ELSE IF (answer .EQ. '&') THEN
            status = 5
            ierr = 0

         ELSE IF (answer .EQ. '??') THEN
            status = 6
            ierr = 0

	 ELSE IF (answer .EQ. '???') THEN
	    status = 7
	    ierr = 0

         ELSE IF ( ANSWER .EQ. ' ' ) THEN					!  If carriage return.

            IF (DEFIN) THEN
               DVALUE = DEFAULT							!   Default is required.
               IERR = 0								!   Indicate valid answer.
               STATUS = 0
            ELSE
               WRITE (*,'(A,$)') ' Invalid reply, please enter real value: '
            END IF

        ELSE									!  Else any other input.

          STATUS = 0

          DOT = .FALSE.
          I = 1

          DO WHILE ( I .LE. 20 .AND. .NOT. DOT )

            IF ( ANSWER( I:I ) .EQ. '.' ) DOT = .TRUE.
            IF ( ANSWER( I:I ) .NE. ' ' ) PLACE = I
            I = I + 1

          END DO

          IF ( .NOT. DOT ) THEN

            PLACE = PLACE + 1
            ANSWER( PLACE:PLACE ) = '.'

          END IF

          READ( ANSWER , '( F )' , IOSTAT = IERR ) DVALUE
          IF ( IERR .NE. 0 ) WRITE (*,'(A,$)') ' Invalid reply, please enter real value or <CR>: '

        END IF

      END DO

      END
