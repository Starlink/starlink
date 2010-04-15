      SUBROUTINE PGVCLR( X1, X2, Y1, Y2, STATUS )
*+
*  Name:
*     PGVCLR

*  Purpose:
*     To clear a retangular area of a PGPLOT viewport.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGVCLR( X1, X2, Y1, Y2, STATUS )

*  Description:
*     If the device is not a hardcopy device then
*     the routine first tests to see if the current device has a
*     background colour with which to erase the contents of the
*     rectangular area defined by the two sets of corner world
*     coordinates (x1,y1), (x2,y2). If the test is successful then the
*     current values for colour and fill area style are stored. The
*     corner coordinates are then used to clear a retangle. The colour
*     and fill area styles are then set to erase all the contents of
*     the area and the area is erased. Finally the entrant values of
*     colour and fill style are restored.

*  Arguments:
*     X1 = REAL (Given)
*        X value of one of the corners defining the rectangle.
*     X2 = REAL (Given)
*        X value of one of the corners defining the rectangle.
*     Y1 = REAL (Given)
*        Y value of one of the corners defining the rectangle. This
*        value must correspond to the X1 value.
*     Y2 = REAL (Given)
*        Y value of one of the corners defining the rectangle. This
*        value must correspond to the X2 value.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER OLDCOL             ! entrant colour index
      INTEGER OLDFIL             ! entrant fill area style
      INTEGER IL                 ! Length of the returned string
      CHARACTER*3 DEVTYP         ! device type, no clear if hardcopy

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out if the device is a hardcopy device
*  if so do not clear the plot, saves a lot of time
*  in the laser printer queue
      CALL PGQINF( 'HARDCOPY', DEVTYP, IL )
      IF( DEVTYP( 1 : 1 ) .NE. 'Y' ) THEN

*  Have a background colour so can erase, store the present set up
*  for colour and fill style
         CALL PGQCI( OLDCOL )
         CALL PGQFS( OLDFIL )

*  Set the colour index, 0 represents the background colour
         CALL PGSCI( 0 )

*  Set the area fill style, 1 represents solid
         CALL PGSFS( 1 )

*  Fill the rectangular area
         CALL PGRECT( X1, X2, Y1, Y2 )

*  Reset the entrant colour and fill style
         CALL PGSCI( OLDCOL )
         CALL PGSFS( OLDFIL )
      END IF
      END
* $Id$
