      PROGRAM GWM_TEST
*+
*  Name:
*     GWM_TEST

*  Purpose:
*     Test the GWM FORTRAN interface

*  Language:
*     FORTRAN

*  Description:
*     Most of the functionality of the GWM FORTRAN interface
*     is tested by this program.

*  Authors:
*     NE: Nick Eaton (Durham University)
*     BLY: M.J.Bly  (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1991 (NE):
*        Original version.
*     26-FEB-1998 (BLY):
*        Initialise STATUS to SAI__OK.
*     13-MAR-2004 (TIMJ):
*        Exit with bad shell status is STATUS not SAI__OK
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Constants:
      INTEGER IDIM
      PARAMETER ( IDIM = 64 )
      CHARACTER * ( * ) WNAME
      PARAMETER ( WNAME = 'XWINDOWS' )

*  Local Variables:
      INTEGER INDEXS( IDIM ), NCOLS, STATUS
      LOGICAL EXISTS
*.
*   Set STATUS to SAI__OK
      STATUS = SAI__OK

*   Open the default X device
      CALL GWM_OPEN( ' ', .TRUE., STATUS )

*   Set up the size and position of the window
      CALL GWM_WSETI( 'WIDTH', 256, STATUS )
      CALL GWM_WSETI( 'HEIGHT', 192, STATUS )
      CALL GWM_WSETI( 'BORDERWIDTH', 100, STATUS)
      CALL GWM_WSETI( 'XORIGIN', 0, STATUS )
      CALL GWM_WSETI( 'YORIGIN', 0, STATUS )
      CALL GWM_WSETI( 'XSIGN', -1, STATUS)
      CALL GWM_WSETI( 'YSIGN', -1, STATUS)

*   Allocate 128 colours to this window
      CALL GWM_WSETI( 'COLOURS', 12, STATUS )

*   Define the foreground and background colours
      CALL GWM_WSETC( 'FOREGROUND', 'White', STATUS )
      CALL GWM_WSETC( 'BACKGROUND', 'DarkSlateGrey', STATUS )

*   Give the window a title
      CALL GWM_WSETC( 'TITLE', 'Small_window', STATUS )

*   Create the GWM window
      CALL GWM_CRWIN( WNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that the window exists
      CALL GWM_EXIST( WNAME, EXISTS, STATUS )

*   Print out a message
      IF ( EXISTS ) THEN
         WRITE(*,'(1X,''Window '',A,'' created'')' ) WNAME
      ELSE
         WRITE(*,'(1X,''Window '',A,'' does not exist.'')' ) WNAME
         GOTO 99
      ENDIF

*   Inquire the number of allocated colours
      CALL GWM_GETCI( WNAME, IDIM, INDEXS, NCOLS, STATUS )

*   Print out a message
      IF ( STATUS .EQ. SAI__OK ) THEN
         WRITE(*,'(1X,''with '',I3,'' colours allocated.'')' ) NCOLS
      ENDIF

*   Destroy the window
      CALL GWM_DSWIN( WNAME, STATUS )

*   Print out a message
      IF ( STATUS .EQ. SAI__OK ) THEN
         WRITE(*,'(1X,''Window '',A,'' destroyed.'')' ) WNAME
      ENDIF

  99  CONTINUE
      CALL GWM_CLOSE( STATUS )

*   Exit with bad error status to shell if we have an error
*   so that the test system knows we have failed
      IF (STATUS .NE. SAI__OK) CALL EXIT( 1 )

      END

