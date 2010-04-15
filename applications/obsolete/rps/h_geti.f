*+H_GETI           Gets an integer answer from the user, or  default
*  History
*     1989 Jan	M Ricketts	1st vsn, based on MDH_GETI
*     1992 Apr  M. Duesterhaus	remove VAX specific code
*     1993 Jun  P. Brisco       Added # option to skip similar fields.
*     1993 Jun  P. Brisco       Added & option to go to previous field.
*     1993 Jun  P. Brisco       Added ?? option to list key help.
*     1993 Jul	P. Brisco	Added ??? option to list menu help.
*************************************************************************
      SUBROUTINE H_GETI( QUERY , DEFIN, DEFAULT, IVALUE, STATUS)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) QUERY   	! In	Question asked.
      LOGICAL 	DEFIN		!	True if default
      INTEGER       DEFAULT     ! 	Default value.
      INTEGER IVALUE
      INTEGER STATUS


*  Local Variables
      CHARACTER*11  ANSWER     	! Reply recieved.
      INTEGER       IERR        ! Error indicator.
      INTEGER            J           ! Length of answer.
     & ,            LEN 	! Length of query.
      INTEGER NCHS
      CHARACTER*128 DEFSTRING

*  Functions
      INTEGER MDH_ENDWORD        ! Finds last non blank character in string.

*  ____________________________ Executable Code ____________________________

      IF (DEFIN) THEN
         IF ( DEFAULT .GT. 0 ) THEN						! If default value is greater than 0.
            J = INT( LOG10( REAL( DEFAULT ) ) + 1 )				!  Length is log of default.
         ELSE IF ( DEFAULT .EQ. 0 ) THEN					! Else if default is 0.
            J = 1								!  Length is obviously 1.
         ELSE									! Else if default negative.
            J = INT ( LOG10 ( REAL ( - DEFAULT ) ) + 2 )			!  Length is log of - default + 1 for - sign.
         END IF									! End if.
         WRITE(DEFSTRING, '(A,I4,A)') '(Default= ', DEFAULT, ')'
         NCHS = J+14
      ELSE
         DEFSTRING = ' '
         NCHS = 1
      END IF

      LEN = MDH_ENDWORD( QUERY )
      WRITE( * , '( X,A,$)' )
     &        QUERY( :LEN ) //' '// DEFSTRING(:NCHS) //': '			! Ask question and give default.-
      IERR = 1									! Set error indicator.

      DO WHILE ( IERR .NE. 0 )							! Do until valid answer.
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
               IVALUE = DEFAULT							!   Default is required.
               IERR = 0								!   Indicate valid answer.
               STATUS = 0
            ELSE
               WRITE (*,'(A,$)') ' Invalid reply, please enter integer value: '	!   If error ask for input again.
            END IF

        ELSE									!  Else any other input.

          STATUS = 0
          READ ( ANSWER( :MDH_ENDWORD( ANSWER ) )				!   Find length of answer so as to
     &     ,          '( I )' , IOSTAT = IERR ) IVALUE				!   transfer answer to return value.
          IF ( IERR .NE. 0 ) WRITE (*,'(A,$)') ' Invalid reply, please enter integer value: '	!   If error ask for input again.

        END IF									!  End if.

      END DO 									! End do.

      END                                                                        ! Answer got.
