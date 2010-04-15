      SUBROUTINE KPS1_FLXYR( NAME, NVERT, IPX, IPY, STATUS )
*+
*  Name:
*     KPS1_FLXYR

*  Purpose:
*     Obtains a list of co-ordinates from a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_FLXYR( NAME, NVERT, IPX, IPY, STATUS )

*  Description:
*     This routine obtains a list of 2-dimensional co-ordinates for a
*     series of positions by calling KPG1_FLCOR. Pointers to workspace
*     containing the coordinates are returned. These pointers should be
*     released by calling PSX_FREE when they are no longer needed.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the parameter with which to associate the text file.
*     NVERT = INTEGER (Returned)
*        The number of co-ordinate sets specified in the text file.
*     IPX = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NVERT
*        holding the X co-ordinates of each point.
*     IPY = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NVERT
*        holding the Y co-ordinates of each point.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is just a wrap-up of KPG1_FLCOR to get round the
*     problems caused by the original version being removed from KAPPA.
*     -  The number of points read from the file is reported at the
*     normal reporting level.

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     2-OCT-1996 (DSB):
*        New version written to replace the original version which
*        has been removed from KAPPA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER NAME*(*)

*  Arguments Returned:
      INTEGER NVERT
      INTEGER IPX
      INTEGER IPY

*  Inherited Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER POSCOD( 2 )
      INTEGER IPXY
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )

*  Check the inherited status
      if( STATUS .NE. SAI__OK ) RETURN

*  Set up the columns in which the X and Y values are expected to be
*  stored.
      POSCOD( 1 ) = 1
      POSCOD( 2 ) = 2

*  Call the genuine KAPPA routine to get the coordinate data.
      CALL KPG1_FLCOR( NAME, 2, POSCOD, NVERT, IPXY, LBND,
     :                 UBND, STATUS )

*  If any positions were given, get two work arrays to hold the x and
*  y co-ordinates of the vertices.
      IF( NVERT .GT. 0 ) THEN
         CALL PSX_CALLOC( NVERT, '_REAL', IPX, STATUS )
         CALL PSX_CALLOC( NVERT, '_REAL', IPY, STATUS )

*  Split the 2-dimensional array into the x-y arrays.
         CALL KPG1_UNZ2R( NVERT, %VAL( IPXY ), %VAL( IPX ), %VAL( IPY ),
     :                 STATUS )

      END IF

*  Free the original array.
      CALL PSX_FREE( IPXY, STATUS )

      END
