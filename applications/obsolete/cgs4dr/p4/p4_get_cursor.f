*+  P4_GET_CURSOR - Position cursor, return co-ords and keystroke,
      SUBROUTINE P4_GET_CURSOR( PORT, STATUS )
*    Invocation :
*     CALL P4_GET_CURSOR( PORT, STATUS )
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     P. N. Daly (JACH::PND)
*    History :
*     1989:        Original version.                                (JFL)
*     24-Oct-1989: History added. Further status checks added.
*                  Superfluous call to DSA_CLOSE removed.           (SMB)
*     25-Oct-1989: Setting of status to ACT_END removed, so that
*                  P4_ENDACTION may be used.                        (SMB)
*      2-Nov-1989: Status check after PGPOINT improved. Warning
*                  issued if the cursor is outside the plot window. (SMB)
*     16-Feb-1990: CURSOR_STATUS parameter added, so that ICL
*                  procedures can stop if something has gone wrong. (SMB)
*     16-Feb-1990: Bug fix. Check on window limits was incorrect.   (SMB)
*     20-Jul-1990: COLUMNS added for HISTOGRAM.                     (SMB)
*     10-Feb-1991: Bug which caused this routine to crash if called
*                  without a device open fixed.                     (SMB)
*     18-Feb-1993: Tidy code                                        (PND)
*     12-Aug-1994: Major modifications for Unix port                (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PORT                            ! Port number
*    External references :
      INTEGER PGCURSE
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
*    Local variables :
      LOGICAL UPDATE                          ! T if update plot before curse
      REAL X, Y                               ! Cursor position
      CHARACTER*1 KEYSTROKE                   ! Keystroke
      INTEGER ERR_STAT                        ! An error status
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the CURSOR_STATUS parameter to "bad" (non-zero).
      CALL PAR_PUT0I( 'CURSOR_STATUS', 1, STATUS )

*    Check that this port has a valid plot in it
      IF ( .NOT. PLOT_OK( PORT ) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_GET_CURSOR: '/
     :    /'There is no valid plot in this viewport', STATUS )
      ENDIF

*    Check that this is an IMAGE, GRAPH or HISTOGRAM
      IF ( ( DISPLAY_TYPE( PORT ) .NE. 'IMAGE' )     .AND.
     :     ( DISPLAY_TYPE( PORT ) .NE. 'HISTOGRAM' ) .AND.
     :     ( DISPLAY_TYPE( PORT ) .NE. 'GRAPH' ) )   THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_GET_CURSOR: '/
     :    /'Display must contain IMAGE, GRAPH or HISTOGRAM', STATUS )
      ENDIF

*    Get update information and redo plot
      CALL PAR_GET0L( 'UPDATE', UPDATE, STATUS )
      IF ( UPDATE ) THEN
        IF ( DISPLAY_TYPE( PORT ) .EQ. 'IMAGE' ) THEN
          CALL P4_IMAGE( PORT, STATUS )
        ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' ) THEN
          CALL P4_GRAPH( PORT, STATUS )
        ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'HISTOGRAM' ) THEN
          CALL P4_HISTOGRAM( PORT, STATUS )
        ENDIF
      ELSE
        CALL P4_SELWINDOW( PORT, STATUS )
      ENDIF

*    Centre the cursor in the window, PGCURSE moves it to specified position
      X = (XSTART( PORT ) + XEND( PORT ) ) / 2.0
      Y = (YSTART( PORT ) + YEND( PORT ) ) / 2.0

*    Get cursor position
      STATUS = PGCURSE( X, Y, KEYSTROKE )

*   If an error occurs, a bad status or an ASCII null character will be returned
      IF ( ( STATUS .NE. PG__OK ) .OR. ( KEYSTROKE .EQ. CHAR( 0 ) ) ) THEN
        ERR_STAT = STATUS
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'ES', ERR_STAT )
        CALL ERR_REP( ' ', 'P4_GET_CURSOR: '/
     :    /'PGPLOT error while reading cursor, Status = ^ES', STATUS )
      ELSE

*      All OK, mark the point
        CALL ERR_ANNUL( STATUS )
        CALL PGPOINT( 1, X, Y, 5 )

*      Issue a warning if the cursor position is outside the plot window.
        IF ( ( X .LT. XSTART( PORT ) ) .OR.
     :       ( X .GT. XEND( PORT ) )   .OR.
     :       ( Y .LT. YSTART( PORT ) ) .OR.
     :       ( Y .GT. YEND( PORT ) ) ) THEN

          CALL MSG_OUT( ' ', 'Cursor is outside plot window !', STATUS )
        END IF

*      Write out the X, Y and KEYSTROKE parameters
        CALL PAR_PUT0R( 'X', X, STATUS )
        CALL PAR_PUT0R( 'Y', Y, STATUS )
        CALL CHR_UCASE( KEYSTROKE )
        CALL PAR_PUT0C( 'KEYSTROKE', KEYSTROKE, STATUS )

*      If the parameters have been updated successfully,CURSOR_STATUS set ok.
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL PAR_PUT0I( 'CURSOR_STATUS', 0, STATUS )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'P4_GET_CURSOR: '/
     :      /'Error writing X, Y and KEYSTROKE parameters', STATUS )
        ENDIF
      ENDIF

      END
