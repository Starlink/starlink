      SUBROUTINE IMG1_NMEX( SLOT, ESLOT, N, STATUS )
*+
* Name:
*    IMG1_NMEX

*  Purpose:
*     Returns the number of extension items.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_NEX( SLOT, ESLOT, N, STATUS )

*  Description:
*     This routine returns the number of primitive objects in an
*     extension. It uses the values stored in the ECB common blocks to
*     determine this, so the extension must be traced (using IMG1_TRACE)
*     prior to calling this routine.

*  Arguments:
*     SLOT = INTEGER (Given)
*        NDF slot number.
*     ESLOT = INTEGER (Given)
*        Extension slot number.
*     N = INTEGER (Returned)
*        The number of items.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The N'th extension item is really the N'th primitive object,
*     this might not be what's required if only scalar primitives are
*     needed.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_XNSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read)
*        The number of locators in an extension stack.

*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT

*  Arguments Returned:
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just pass back the required number.
      N = ECB_XNSTK( SLOT, ESLOT )

      END
* $Id$
