*+  MDH_GETL
*-  Author M.D.C.Harris ( R.A.L )                    11th June 1987.
*************************************************************************
      LOGICAL FUNCTION MDH_GETL( QUERY , DEFAULT )

*  -----------
*  DESCRIPTION
*  -----------

*  This function returns a logical answer to a question.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) QUERY    	! Question.
      LOGICAL       DEFAULT     ! Default answer.

*LOCAL:

      CHARACTER*3   ANSWER     	! Users input.
      INTEGER       IANS        ! Indicates if string is positive or negative.
     & ,            LEN 	! Length of query.
     & ,            NBLANKS	! Number of blank lines at end of command.
     & ,            I 		! Number of blank lines at start.
      LOGICAL       IERR        ! Error indicator.

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

      INTEGER MDH_LOGIC        	! Returns positive or negative indicator.
     & ,      MDH_ENDWORD	! Finds end of string.


      NBLANKS = 0
      I = 1

      DO WHILE ( QUERY( I:I ) .EQ. '/' )

        WRITE( * , * )
        I = I + 1

      END DO

      LEN = MDH_ENDWORD( QUERY )

      DO WHILE ( QUERY( LEN:LEN ) .EQ. '/' )
										
        NBLANKS = NBLANKS + 1
        LEN = LEN - 1

      END DO

      IF ( DEFAULT ) THEN                                                       ! If default is true.

        WRITE (* , '( X,A,$ )' ) QUERY( I:LEN ) //' ( Default is YES): '          !  Write question giving default as YES.

      ELSE                                                                      ! Else if default is false.

        WRITE (* , '( X,A,$ )' ) QUERY( I:LEN ) //' ( Default is NO): '          !  Write giving default as NO

      END IF                                                                    ! End if.
                                                                     
      IERR = .TRUE.                                                             ! Set error indicator to no error.

      DO WHILE ( IERR )                                                         ! Do while no valid answer.

        READ (* ,'(A)') ANSWER                                                  !  Read in answer.
        IANS = MDH_LOGIC( ANSWER )                                              !  See if answer is positive or negative.

        IF (ANSWER .EQ. ' ') THEN                                               !  If RETURN key pressed.

          MDH_GETL = DEFAULT                                                    !   Return default.5
          IERR = .FALSE.                                                        !   Valid reply given.

        ELSE IF ( IANS .GT. 0 ) THEN                                            !  Else if answer in affirmative.

          MDH_GETL = .TRUE.                                                     !   Return true.
          IERR = .FALSE.                                                        !   Valid reply.

        ELSE IF ( IANS .LT. 0 ) THEN                                            !  Else if answer is in negative then.

          MDH_GETL = .FALSE.                                                    !   Return false.
          IERR = .FALSE.                                                        !   Valid reply.

        ELSE                                                                    !  Else for any other input.

          WRITE (* , '( A,$ )')	' Please answer YES or NO: '		!   Tell user its invalid.
          IERR = .TRUE.                                                         !   Invalid reply.

        END IF                                                                  !  End if.

      END DO                                                                    ! End do.

      DO I = 1 , NBLANKS

        WRITE( * , * )

      END DO

      END                                                                       ! Return value.
