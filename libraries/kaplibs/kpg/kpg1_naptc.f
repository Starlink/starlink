      SUBROUTINE KPG1_NAPTC( CVALUE, LOC, NDIM, DIM, STATUS )
*+
*  Name:
*     KPG1_NAPTC

*  Purpose:
*     Swaps argument order when putting a mapped character array into
*     an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NAPTC( CVALUE, LOC, NDIM, DIM, STATUS )

*  Description:
*     This is just a dummy routine to swap the argument order when
*     putting the value of a mapped character array into HDS object.  It
*     is needed for Unix systems.

*  Arguments:
*     CVALUE = CHARACTER * ( * ) (Given)
*        The character value.
*     LOC = CHARACTER * (DAT__SZLOC) (Given)
*        The locator of the object to have value CVALUE.
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     DIM( * ) = INTEGER (Given)
*        The dimensions of the character object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1992 (RFWS):
*        Original version.
*     1992 May 9 (MJC):
*        Renamed from NATPTC and added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * ) CVALUE
      CHARACTER * ( * ) LOC
      INTEGER NDIM
      INTEGER DIM( * )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the character value.
      CALL DAT_PUTC( LOC, NDIM, DIM, CVALUE, STATUS )

      END
