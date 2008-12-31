*+
      SUBROUTINE MDH_CHECK( VALIN , FORMAT , VALOUT , OKAY )

*DESCRIPTION:

*     Checks the format of data string and returns a correctly formatted 
*     string if possible if not it returns a message.

*INPUT:

      CHARACTER*(*) FORMAT	! Required format.
     & ,            VALIN	! Input data.

*OUTPUT:

      CHARACTER*(*) VALOUT	! Output value.
      LOGICAL       OKAY	! Indicates if correct formatting is possible.

*LOCAL:

      INTEGER       IERR	! Error indicator.
     & ,            ILOG
     & ,            I , J	! Position marker.
     & ,            IVAL	! Temporary variables.
     & ,            exp
      LOGICAL       LVAL
      REAL          RVAL
      DOUBLE PRECISION DVAL

*FUNCTIONS:

      INTEGER MDH_ENDWORD	! Gets the last non blank characters position.
     & ,      MDH_LOGIC		! Returns a value indicating the logical equivalent of the string.

*SUBROUTINES REFERENCED:

*-  Author M.D.C.Harris ( R.A.L )                    30th March 1987.

      J = MDH_ENDWORD( VALIN )							! Get length of string itself.

      IF ( J .GE. 1 ) THEN							! Initialise return string.
                       
        VALOUT = VALIN( : J )

      ELSE

        J = 1
        VALOUT = ' '

      END IF

      IF ( INDEX( FORMAT , 'A' ) .NE. 0 ) THEN					! If character format then.

        IERR = 0								!  No mistake in format possible.

      ELSE IF ( INDEX( FORMAT , 'F'  ) .NE. 0 .OR.
     &        ( INDEX( FORMAT , 'D'  ) .NE. 0 .AND.
     &          INDEX( FORMAT , 'DD' ) .EQ. 0 ) ) THEN				! Else if real format.

        I = MAX( INDEX( VALIN , 'D' ) , INDEX( VALIN , 'E' ) ) + 1

        IF ( I .GT. 1 ) THEN
                                            
          READ( VALIN( I:J ) , '( I )' ) EXP

          IF ( ABS( EXP ) .GT. 38 ) THEN

            OKAY = .FALSE.
            RETURN

          END IF

          J = I - 1
          VALOUT( J: ) = ' '

        ELSE

          J = J + 1               

        END IF

        IF ( INDEX( VALIN , '.' ) .EQ. 0 .AND. J .LT. LEN( VALIN ) ) 	!  Add .0 if not there so as to get correct format -
     &       VALOUT( J: ) = '.0'							!  if the string has a numerical content.
        READ( VALOUT , '( D )' , IOSTAT = IERR ) DVAL				!  See if string is real and if so -

        IF ( IERR .EQ. 0 ) THEN

          IF ( EXP .NE. 0 ) DVAL = DVAL * 10.0 ** EXP
          WRITE( VALOUT , '(' // FORMAT // ')' , IOSTAT = IERR ) DVAL	!  put it into the return string.

        END IF

      ELSE IF ( INDEX( FORMAT , 'I' ) .NE. 0 ) THEN				! Else if integer format then.

        I = MAX( INDEX( VALIN , 'D' ) , INDEX( VALIN , 'E' ) ) + 1

        IF ( I .GT. 1 ) THEN
                                            
          READ( VALIN( I:J ) , '( I )' ) EXP

          IF ( EXP .GT. 9 .OR. EXP .LT. 0 ) THEN

            OKAY = .FALSE.
            RETURN

          END IF
                        
          J = I - 2
          VALOUT( J + 1: ) = ' '

        END IF

        READ ( VALOUT( :J ) , '( I )' , IOSTAT = IERR ) IVAL			!  Find length of answer so as to

        IF ( IERR .EQ. 0 ) THEN							!  transfer answer to return value.

          IF ( EXP .GT. 0 ) IVAL = IVAL * 10 ** EXP
          WRITE( VALOUT , '(' // FORMAT // ')' , IOSTAT = IERR ) IVAL
                          
        END IF

      ELSE IF ( INDEX( FORMAT , 'L' ) .NE. 0 ) THEN				! Else if logical then.

        ILOG = MDH_LOGIC( VALIN )						!  Get indicator of logical equivalent.

        IF ( ILOG .GT. 0 ) THEN							!  If equivalent is true then.

          IERR = 0
          LVAL = .TRUE.								!   Return true.
          VALOUT = 'Y'

        ELSE IF ( ILOG .LT. 0 ) THEN						!  Else if false then.

          IERR = 0                               
          LVAL = .FALSE.							!   Return false.
          VALOUT = 'N'

        ELSE									!  Else if no equivalent then.

          IERR = 1								!   Return error.

        END IF									!  Fi.

C        IF ( IERR .EQ. 0 )
C     &     WRITE( VALOUT , '(' // FORMAT // ')' , IOSTAT = IERR ) LVAL	!  Return corrected value.
                                
      ELSE IF ( INDEX( FORMAT , 'HHMMSS' ) .NE. 0 ) THEN				! Else if hours minuetes seconds then.

        J = J + 1

        IF ( INDEX( VALIN , '.' ) .EQ. 0 .AND. J .LT. LEN(VALIN) ) THEN		!  Add .0 if not there so as to get correct format -

          VALOUT( J: ) = '.0'							!  if the string has a numerical content.
 
        END IF

        READ ( VALOUT , '(F)' , IOSTAT = IERR ) RVAL				!  See if string is real and if so -
        
        IF ( RVAL .GT. 240000.0 .OR. RVAL .LT. 0.0 .OR.
     &       RVAL - INT( RVAL / 10000 ) * 10000 .GT. 5959.999 .OR. 
     &       RVAL - INT( RVAL / 100 ) * 100 .GT. 59.999 ) THEN

          IERR = 1

        ELSE

          IERR = 0

        END IF

      ELSE IF ( INDEX( FORMAT , 'DDMMSS' ) .NE. 0 ) THEN				! Else if hours minuetes seconds then.

        J = J + 1

        IF ( INDEX( VALIN , '.' ) .EQ. 0 .AND. J .LT. LEN(VALIN) ) THEN		!  Add .0 if not there so as to get correct format -

          VALOUT( J: ) = '.0'							!  if the string has a numerical content.
 
        END IF

        READ ( VALOUT , '(F)' , IOSTAT = IERR ) RVAL				!  See if string is real and if so -
        
        RVAL = ABS( RVAL )
        IF ( RVAL .GT. 900000.0 .OR.
     &       RVAL - INT( RVAL / 10000 ) * 10000 .GT. 5959.999 .OR. 
     &       RVAL - INT( RVAL / 100 ) * 100 .GT. 59.999 ) THEN

          IERR = 1

        ELSE

          IERR = 0

        END IF

      END IF									! Fi.

      IF ( IERR .EQ. 0 ) THEN							! Return correct error.

        OKAY = .TRUE.

      ELSE

        OKAY = .FALSE.

      END IF

      END									! End.
