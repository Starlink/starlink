*+  P4_SELWINDOW - Select a viewport on the device display surface and
*                  map the window onto it that was last used
      SUBROUTINE P4_SELWINDOW( PORT, STATUS )
*    Invocation :
*     CALL P4_SELWINDOW( PORT, STATUS )
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989: Original version (JFL)
*      4-Aug-1994: Convert to I-task for Unix port (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER PORT                        ! the number of the port
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that viewport has had something plotted in it
      IF ( PLOT_OK(PORT) .EQ. .FALSE. ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_SELWINDOW: '/
     :    /'Viewport has not been plotted to', STATUS )
      ELSE

*    Select the viewport, using the actual limits stored by plotting subroutine
        CALL PGVPORT( AXSTART( PORT ), AXEND( PORT ),
     :    AYSTART( PORT ), AYEND( PORT ) )

*    Put the window into it
        CALL PGWINDOW( XSTART( PORT ), XEND( PORT ),
     :    YSTART( PORT ), YEND( PORT ) )
      ENDIF

      END
