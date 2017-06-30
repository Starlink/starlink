      SUBROUTINE POL1_CPVRC( CIIN, CIOUT, STATUS )
*+
*  Name:
*     POL1_CPVRC

*  Purpose:
*     Copy the POLPACK version number to a new catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPVRC( CIIN, CIOUT, STATUS )

*  Description:
*     This routine extracts the version number from the VERSION parameter
*     of the supplied input catalogue, and writes it to the output
*     catalogue.

*  Arguments:
*     CIIN = INTEGER (Given)
*        A CAT identifier for the input catalogue.
*     CIOUT = INTEGER (Given)
*        A CAT identifier for the output catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUN-2017 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER VERS*20          ! Version string
      INTEGER GI                 ! CAT identifier for VERSION parameter
      INTEGER VLEN               ! Used length of VERS
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a CAT identifier for the VERSION parameter in the input catalogue.
      CALL CAT_TIDNT( CIIN, 'VERSION', GI, STATUS )

*  Version tagging was only introduced at version 2.0-2. If there is no
*  version number in the extension, annul the error and assume V1.0.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         VERS = '1'

*  Otherwise, get the parameters value, and release the identifier.
      ELSE
         CALL CAT_TIQAC( GI, 'VALUE', VERS, STATUS )
         CALL CAT_TRLSE( GI, STATUS )
      END IF

*  Create the output catalogue parameter.
      VLEN = CHR_LEN( VERS )
      CALL CAT_PPTSC( CIOUT, 'VERSION', VERS( : VLEN ),
     :                'POLPACK version', GI, STATUS )

*  Truncate the string to the correct length.
      CALL CAT_TATTI( GI, 'CSIZE', VLEN, STATUS )

*  Release the parameter identifier.
      CALL CAT_TRLSE( GI, STATUS )

      END
