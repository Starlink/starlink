*-----------------------------------------------------------------------
*+  IKNBLM - Blink memories

      SUBROUTINE IKNBLM ( DISPID, MEMID, NMEM, BLINKS, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMBLM.
*     The arguments are identical to those in IIMBLM.
*
*    Invocation :
*     CALL IKNBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments. Then blink the memories using an
*     non-obvious Ikon command, Set Frame Grab Control Latch.
*     The blinking is continued until the exit button is pressed.
*     Pressing the left hand button increases the blinking speed
*     and pressing the centre button decreases the speed.
*     At the end of the routine reset the display to normal.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*     VAX specific calls - LIB$WAIT
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     April 1989
*     November 1989  Added use of mouse buttons.
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     List of memory identifiers
      INTEGER MEMID( * )

*     Number of memory identifiers
      INTEGER NMEM

*     Blink periods ( in seconds )
      REAL BLINKS( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'

*    Local constants :
*     Number of tries to clear out type-ahead button presses
      INTEGER MAXCLR
      PARAMETER ( MAXCLR = 100 )

*     Scaling factor for blink period
      REAL FACTOR
      PARAMETER ( FACTOR = 4.0 / 3.0 )

*    Local variables :
      LOGICAL IDTCON, STATE( 0 : 2 ), TRIGS( 0 : 2 )

      INTEGER * 2 PRESS, WORDS( 2 )

      INTEGER BUTTON( 0 : 2 ), I, J, K, NUMWOR

*    Local data
      DATA BUTTON / 16384, 8192, 4096 /
*-

*   Recover the common blocks if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Check the number of memories is valid
      IF ( ( NMEM .LT. 1 ) .OR. ( NMEM .GT. CNMEM ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Check the memory identifiers are valid
      DO J = 1, NMEM
         IF ( ( MEMID( J ) .LT. 0 ) .OR.
     :        ( MEMID( J ) .GE. CNMEM ) ) THEN
            STATUS = IDI__INMID
            GOTO 99
         ENDIF
      ENDDO

*   Clear out any residual button presses from the mouse
*   Only do this a limited number of times ( MAXCLR ) since the user
*   may be holding down the mouse button
      I = 0
      PRESS = 1
      DO WHILE ( ( PRESS .NE. 0 ) .AND. ( I .LT. MAXCLR ) )

*   Ikon command 94 = '5E'X = Return menu status
         WORDS( 1 ) = 94
         NUMWOR = 1
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )
         NUMWOR = 1
         CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )
         I = I + 1
      ENDDO

*   The STATE array determines if the button press will be obeyed.
*   It is only reset when a button is released which ensures that
*   holding down a button does not result in multiple button presses.
      DO K = 0, 2
         TRIGS( K ) = .FALSE.
         STATE( K ) = .TRUE.
      ENDDO

*   Loop until the exit button is pressed
      DO WHILE ( .NOT. TRIGS( 2 ) )

*   Display the memories in turn
         DO J = 1, NMEM

*   Select the memory to display
*   Ikon command 92 = '5C'X = Set Frame Grab Control Latch
            WORDS( 1 ) = 92
            IF ( MEMID( J ) .EQ. 0 ) THEN
               WORDS( 2 ) = 3
            ELSEIF( MEMID( J ) .EQ. 1 ) THEN
               WORDS( 2 ) = 0
            ELSE
               WORDS( 2 ) = 1
            ENDIF
            NUMWOR = 2
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            CALL IKNOUT( STATUS )

*   Wait for the requested time.
*   If the blink period is less than a 1/4 a second then just call
*   a system wait
            IF ( BLINKS( J ) .LT. 0.25 ) THEN
               CALL LIB$WAIT( BLINKS( J ) )

*   Otherwise poll the mouse buttons during the blink to allow the
*   user to break out of a long blink. IDTBEG starts the timer and
*   IDTCON returns false when the timer has finished.
            ELSE
               CALL IDTBEG( BLINKS( J ) )
               DO WHILE( IDTCON() )

*   See if any buttons have been pressed
*   Ikon command 94 = '5E'X = Return menu status
                  WORDS( 1 ) = 94
                  NUMWOR = 1
                  CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
                  CALL IKNOUT( STATUS )
                  NUMWOR = 1
                  CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )
                  DO K = 0, 2
                     IF ( IAND( PRESS, BUTTON( K ) ) .NE. 0 ) THEN
                        TRIGS( K ) = .TRUE.
                     ELSE
                        TRIGS( K ) = .FALSE.
                        STATE( K ) = .TRUE.
                     ENDIF
                  ENDDO

*   Jump out of the loop if any button has been pressed
                  DO K = 0, 2
                     IF ( TRIGS( K ) .AND. STATE( K ) ) THEN
                        GOTO 10
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDDO

*   See if any buttons have been pressed
*   Ikon command 94 = '5E'X = Return menu status
         WORDS( 1 ) = 94
         NUMWOR = 1
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )
         NUMWOR = 1
         CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )
         DO K = 0, 2
            IF ( IAND( PRESS, BUTTON( K ) ) .NE. 0 ) THEN
               TRIGS( K ) = .TRUE.
            ELSE
               TRIGS( K ) = .FALSE.
               STATE( K ) = .TRUE.
            ENDIF
         ENDDO

*   If the left hand or centre buttons have been pressed then change
*   the blinking speed.
*   Set the state to false to stop any action with this button until
*   the button is released.
  10     CONTINUE
         IF ( TRIGS( 0 ) .AND. STATE( 0 ) ) THEN
            DO J = 1, NMEM
               BLINKS( J ) = BLINKS( J ) / FACTOR
            ENDDO
            STATE( 0 ) = .FALSE.
         ELSEIF ( TRIGS( 1 ) .AND. STATE( 1 ) ) THEN
            DO J = 1, NMEM
               BLINKS( J ) = BLINKS( J ) * FACTOR
            ENDDO
            STATE( 1 ) = .FALSE.
         ENDIF

      ENDDO

*   Set the display back to its usual configuration
*   Ikon command 92 = '5C'X = Set Frame Grab Control Latch
      WORDS( 1 ) = 92
      WORDS( 2 ) = 1
      NUMWOR = 2
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   Clear out any residual button presses from the mouse
*   Only do this a limited number of times ( MAXCLR ) since the user
*   may be holding down the mouse button
      I = 0
      PRESS = 1
*      DO WHILE ( ( PRESS .NE. 0 ) .AND. ( I .LT. MAXCLR ) )
*   It is better to wait for ever for the button to be released,
*   otherwise the Ikon can get stuck.
      DO WHILE ( PRESS .NE. 0 )

*   Ikon command 94 = '5E'X = Return menu status
         WORDS( 1 ) = 94
         NUMWOR = 1
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )
         NUMWOR = 1
         CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )
         I = I + 1
      ENDDO

  99  CONTINUE

      END

