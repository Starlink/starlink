      SUBROUTINE CCD1_CEXT( ID, CREATE, ACCESS, LOC, STATUS )
*+
*  Name:
*     CCD1_CEXT

*  Purpose:
*     Checks the CCDPACK extension in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CEXT( ID, CREATE, ACCESS, LOC, STATUS )

*  Description:
*     The routine first checks if the CCDPACK extension exists. If
*     it does then a locator is returned to it. If the extension does
*     not exist then either one is created or an error is reported.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of NDF.
*     CREATE = LOGICAL (Given)
*        Whether to create an extension if ones does not exists.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The extension access mode: 'READ', 'WRITE', 'UPDATE'.
*     LOC = CHARACTER * ( * ) (Returned)
*        Locator to CCDPACK extension of NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1992 (PDRAPER):
*        Original version.
*     14-JUN-1993 (PDRAPER):
*        Added option to not create extension.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ID
      LOGICAL CREATE
      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the extension exists.
      CALL NDF_XSTAT( ID, 'CCDPACK', THERE, STATUS )

*  If the extension does not exist then create it if allowed.
      IF ( .NOT. THERE .AND. CREATE ) THEN
         CALL NDF_XNEW( ID, 'CCDPACK', 'CCDPACK_EXT', 0, 0, LOC,
     :                  STATUS )
      ELSE IF ( THERE ) THEN

*  Just get a locator to it.
         CALL NDF_XLOC( ID, 'CCDPACK', ACCESS, LOC, STATUS )
      ELSE

*  Does not exist and we cannot create it. Set status and exit.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_CEXT', '  Extension does not exist',
     :                 STATUS )
      END IF
      END
* $Id$
