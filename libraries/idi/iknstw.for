*-----------------------------------------------------------------------
*+  IKNSTW - Set Transfer Window

      SUBROUTINE IKNSTW ( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
     :                    XOFF, YOFF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMSTW.
*     The arguments are identical to those in IIMSTW.
*
*    Invocation :
*     CALL IKNSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
*    :             XOFF, YOFF, STATUS )
*
*    Method :
*     Verify the input arguments and save them in the common blocks
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     June 1989
*     December 1990  Changed name from IIMSTW
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Identifier for display
      INTEGER DISPID

*     Memory identifier.
      INTEGER MEMID

*     Direction of load. 0 = up. 1 = down.
      INTEGER DIRECN

*     Size of window in X
      INTEGER XSIZE

*     Size of window in Y
      INTEGER YSIZE

*     Data depth. Bits per pixel.
      INTEGER DEPTH

*     Start position of window in X
      INTEGER XOFF

*     Start position of window in Y
      INTEGER YOFF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
*-

*   Recover the characterisitics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   Check the memory identifier is valid
      IF ( ( MEMID .LT. 0 ) .OR. ( MEMID .GT. CNMEM - 1 ) ) THEN
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Store the load direction in the common blocks
      IF ( DIRECN .EQ. 1 ) THEN
         CTWDIR( MEMID ) = 1
      ELSE
         CTWDIR( MEMID ) = 0
      ENDIF

*   Ensure transfer window size is less than the maximum
      IF ( ( XSIZE .GT. 0 ) .AND.
     :     ( XSIZE .LE. CTWDIX( MEMID ) ) ) THEN
         CTWSIX( MEMID ) = XSIZE
      ELSE
         STATUS = IDI__TWOVF
         GOTO 99
      ENDIF
      IF ( ( YSIZE .GT. 0 ) .AND.
     :     ( YSIZE .LE. CTWDIY( MEMID ) ) ) THEN
         CTWSIY( MEMID ) = YSIZE
      ELSE
         STATUS = IDI__TWOVF
         GOTO 99
      ENDIF

*   Depth of transfer window in bits
      IF ( DEPTH .GT. 0 ) THEN
         CTWDE( MEMID ) = DEPTH
      ELSE
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   X and Y offsets of transfer window
      CTWOFX( MEMID ) = XOFF
      CTWOFY( MEMID ) = YOFF

  99  CONTINUE

      END

