*+HGETL            Gets a logical value from the user, or default
*  History
*     1989 Jan	M Ricketts	1st version, based on M Harris's MDH_GETL
*     1993 Jun  P. Brisco       Added # option to skip similar fields.
*     1993 Jun  P. Brisco       Added & option to go to previous field.
*     1993 Jun  P. Brisco       Added ?? option to list help.
*     1993 Jul	P. Brisco	Added ??? option to list menu help.
*************************************************************************
      SUBROUTINE H_GETL(QUERY, DEFIN, DEFAULT, LVALUE, STATUS)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) QUERY    	! In	Question.
      LOGICAL	DEFIN		!	True if default supplied
      LOGICAL       DEFAULT     ! 	Default answer.
      LOGICAL LVALUE		! Out	Value returned
      INTEGER STATUS		!	0: OK, 1: Exit from form,
				!		2: Help required

*  Functions
      INTEGER MDH_LOGIC        	! Returns positive or negative indicator.
     & ,      MDH_ENDWORD	! Finds end of string.

*-

*  Local Variables
      CHARACTER*19 DEFTRUE /', Default is "Yes"'/
      CHARACTER*20 DEFALSE /', Default is "No"'/
      CHARACTER*20 DEFSTRING
      INTEGER NCHS, IOS
      CHARACTER*3   ANSWER     	! Users input.
      INTEGER       IANS        ! Indicates if string is positive or negative.
     & ,            LEN 	! Length of query.
     & ,            I 		! Number of blank lines at start.
      LOGICAL       IERR        ! Error indicator.

*  ________________________  Executable Code  ________________________________

      I = 1

      LEN = MDH_ENDWORD( QUERY )
      IF (DEFIN) THEN
         IF ( DEFAULT ) THEN                                                       ! If default is true.
            DEFSTRING = DEFTRUE
            NCHS = 19
         ELSE
            DEFSTRING = DEFALSE
            NCHS = 20
         END IF
      ELSE
         DEFSTRING = ' '
         NCHS = 1
      END IF
      WRITE(*,'(X,A,X,A,$)' ) QUERY(:LEN), DEFSTRING(:NCHS)//': '
      IERR = .TRUE.                                                             ! Set error indicator to no error.

      DO WHILE ( IERR )                                                         ! Do while no valid answer.

         ANSWER = ' '
         READ( * , '( A )' ,IOSTAT=IOS ) ANSWER				!  Get answer.

         IF (IOS.NE.0) THEN
            STATUS = 1
            IERR = .FALSE.
         ELSE IF (ANSWER .EQ. '?') THEN
            STATUS = 2
            IERR = .FALSE.

         ELSE IF (ANSWER .EQ. '%') THEN
            STATUS = 3
            IERR = .FALSE.

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
               LVALUE = DEFAULT							!   Default is required.
               IERR = .FALSE.							!   Indicate valid answer.
               STATUS = 0
            ELSE
               WRITE (*,'(A,$)') ' Please answer YES or NO: '			!   Tell user its invalid.
            END IF

         ELSE									!  Else any other input.

            STATUS = 0
            IANS = MDH_LOGIC( ANSWER )                                              !  See if answer is positive or negative.

            IF (ANSWER .EQ. ' ') THEN                                               !  If RETURN key pressed.
               LVALUE = DEFAULT                                                    !   Return default.5
               IERR = .FALSE.                                                        !   Valid reply given.

            ELSE IF ( IANS .GT. 0 ) THEN                                            !  Else if answer in affirmative.
               LVALUE = .TRUE.                                                     !   Return true.
               IERR = .FALSE.                                                        !   Valid reply.

            ELSE IF ( IANS .LT. 0 ) THEN                                            !  Else if answer is in negative then.
               LVALUE = .FALSE.                                                    !   Return false.
               IERR = .FALSE.                                                        !   Valid reply.

            ELSE                                                                    !  Else for any other input.
               WRITE (*,'(A,$)') ' Please answer YES or NO: '			!   Tell user its invalid.

            END IF                                                                  !  End if.
         END IF

      END DO                                                                    ! End do.

      END                                                                       ! Return value.
