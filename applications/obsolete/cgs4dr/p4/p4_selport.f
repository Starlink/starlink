*+  P4_SELPORT - Select a subwindow on the device display surface
      SUBROUTINE P4_SELPORT( PORT, STATUS )
*    Invocation :
*     CALL P4_SELPORT( PORT, STATUS )
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     P.N.Daly    (JACH::PND)
*    History :
*     dd-mmm-yyyy: Original version (JFL)
*      5-Aug-1992: Allow variable character sizes (PND)
*     20-Aug-1993: Add FG/BG colours (PND)
*      4-Aug-1994: Convert to I-task and port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER PORT                        ! the number of the port to clear
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
      INCLUDE 'P4COM.INC'                 ! P4 common block
      INCLUDE 'COLOURS.INC'               ! Colour common block
*    Local variables :
      REAL SIZE                           ! Character size
      INTEGER IGNORE
*-

*    Return if status on entry is bad
      IF (STATUS .NE. SAI__OK) RETURN

*    Select the viewport
      CALL PGVPORT( VXSTART( PORT ), VXEND( PORT ),
     :  VYSTART( PORT ), VYEND( PORT ) )

*    Remember it
      CALL PGQVP( 0, AXSTART( PORT ), AXEND( PORT ),
     :  AYSTART( PORT ), AYEND( PORT ) )

*    Scale the character size to the smaller dimension of the new viewport
      SIZE = 2.0 * (MIN(ABS(VXEND(PORT)-VXSTART(PORT)),
     :  ABS(VYEND(PORT)-VYSTART(PORT))))

*    But select bigger size if required
      SIZE = MAX ( SIZE, CHAR_HEIGHT( PORT ) )
      IF ( SIZE .EQ. 0.0 ) SIZE = 0.01
      CALL PGSCH( SIZE )

*    Get and set the FG/BG colours
      CALL P4_COLTONUM( BG_COLOUR( PORT ), IGNORE, BG_RD, BG_GR, BG_BL, STATUS )
      CALL PGSCR( 0, BG_RD, BG_GR, BG_BL )
      CALL P4_COLTONUM( FG_COLOUR( PORT ), IGNORE, FG_RD, FG_GR, FG_BL, STATUS )
      CALL PGSCR( 1, FG_RD, FG_GR, FG_BL )

*    Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        PORT_OK( PORT ) = .FALSE.
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_SELPORT: Unable to select viewport', STATUS )
      ELSE
        PORT_OK( PORT ) = .TRUE.
      ENDIF

      END
