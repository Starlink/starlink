      SUBROUTINE IMG_NEX( PARAM, N, STATUS )
*+
* Name:
*    IMG_NEX

*  Purpose:
*    Returns the number of extensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_NEX( PARAM, N, STATUS )

*  Description:
*     This routine determines the number of extensions in an NDF.  The
*     number returned can be used as an upper limit when indexing using
*     IMG_REXN.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     N = INTEGER (Returned)
*        The number of extensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ parameters

*  Global variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER SLOT               ! Parameter slot number
      LOGICAL WASNEW             ! True if parameter is new
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the parameter name and obtain its slot number.
      CALL IMG1_VPAR( PARAM, VPAR, STATUS )
      CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the number of known extensions from NDF_.
         CALL NDF_XNUMB( PCB_INDF( SLOT ), N, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed in attempt to access the extension, add information to any
*  error messages.
            CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
            CALL ERR_REP( 'IMG_NEX_ERR', 'Failed to determine the ' //
     :           'number of extensions in the NDF ^NDF.', STATUS )
         END IF
      END IF
      END
* $Id$
