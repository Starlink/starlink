*+  P4_CLEARPORT - Clear a subwindow on the device display surface
      SUBROUTINE P4_CLEARPORT( PORT, STATUS )
*    Description :
*     Clears a subwindow on the device display surface, if possible.
*    Invocation :
*     CALL P4_CLEARPORT( PORT, STATUS )
*    Parameters :
*    Method :
*     On devices which are capable of doing this, sub-areas of the
*     screen are cleared by writing them with the background colour.
*     On other devices only the whole surace can be cleared.
*    Deficiencies :
*    Bugs :
*     This routine does not clear areas of the screen consistently.
*     The size seems to vary oddly with the viewport used. Sometimes
*     too large an area is cleared, obliterating the graph next door,
*     and sometimes too small an area is cleared, leaving bits of
*     graphics behind. This bug needs to be fixed properly.
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     P. N. Daly (JACH::PND)
*    History :
*     1989:        Original version.                           (JFL)
*     28-Mar-1990: History added. Documentation improved
*                  and error messages tidied up. More comments
*                  added.                                      (SMB)
*     26-Jul-1990: The routine was not clearing a sufficiently
*                  large area of the screen. Fudge added so that
*                  if the area to be cleared comes within 0.1
*                  of the edge of the screen it is extended
*                  right up to the edge of the screen. However,
*                  this change is just a fudge. I don't yet know
*                  how to fix this problem properly. PGPLOT seems
*                  to regard its viewport as being the area inside
*                  the annotated axis, and the manual says the
*                  there is a margin "4 character heights" all
*                  around used for labelling, which is consistent
*                  with JFL's comments. But the default
*                  character height is only "about 1/40" and
*                  not "exactly 1/40" of the display height.
*                  Perhaps I am missing something, and there is
*                  an easier way to clear the full display area.
*                  "Bugs" section filled in.                 (SMB)
*      6-Apr-1993: For port 0 do not ask user to clear page (KLK/PND)
*      3-Aug-1994: Modified for Unix port                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
*    Local constants :
      INTEGER NPTS
      PARAMETER ( NPTS = 4 )
*    Local data :
      REAL XPTS( NPTS )
      DATA XPTS /0.0,1.0,1.0,0.0/
      REAL YPTS( NPTS )
      DATA YPTS /0.0,0.0,1.0,1.0/
*    Local variables :
      REAL VPX1, VPX2, VPY1, VPY2             ! Limits of port plus margins
      INTEGER PORT
*-

*    Return if status on entry is bad
      IF (STATUS .NE. SAI__OK) RETURN

*    Port 0 is the whole surface, so simply clear the device
      IF ( PORT .EQ. 0 ) THEN
        CALL PGASK( .FALSE. )
        CALL PGPAGE( )
      ELSE

*      Determine the area to be erased
        VPX1 = VXSTART( PORT ) - 0.1 * CHAR_HEIGHT( PORT )
        IF ( VPX1 .LT. 0.1 ) VPX1 = 0.0
        VPX2 = VXEND( PORT ) + 0.1 * CHAR_HEIGHT( PORT )
        IF ( VPX2 .GT. 0.9 ) VPX2 = 1.0
        VPY1 = VYSTART( PORT ) - 0.1 * CHAR_HEIGHT( PORT )
        IF ( VPY1 .LT. 0.1 ) VPY1 = 0.0
        VPY2 = VYEND( PORT ) + 0.1 * CHAR_HEIGHT( PORT )
        IF ( VPY2 .GT. 0.9 ) VPY2 = 1.0

*      Select the area defined as the current viewport
        CALL PGVPORT( VPX1, VPX2, VPY1, VPY2 )

*      Set the colour to colour 0 and fill the area with it.
        CALL PGSCI( 0 )
        CALL PGSFS( 1 )
        CALL PGWINDOW( 0.0, 1.0, 0.0, 1.0 )
        CALL PGPOLY( NPTS, XPTS, YPTS )
        CALL PGSCI( 1 )
      ENDIF

      PLOT_OK ( PORT ) = .FALSE.
      END
